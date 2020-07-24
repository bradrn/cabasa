{-# OPTIONS_GHC -Werror=missing-fields -Werror=missing-methods #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Control.Monad.App
    ( App
    , runApp
    , GtkMouseEvent(..)
    ) where

import Control.Concurrent (killThread, forkIO, threadDelay)
import Data.Int (Int32)
import Data.IORef
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import Foreign.C.Types (CInt)
import Foreign.Ptr (castPtr)
import GHC.TypeLits (KnownNat, natVal)

import CA.ALPACA (AlpacaData(..), runALPACA)
import CA.Universe (Point(..), Coord(..), CARuleA, Universe)
import Control.Monad.Random.Strict (newStdGen, Rand, StdGen)
import Control.Monad.Reader
import qualified Data.Finite as F
import Data.Text (pack)
import qualified Data.Text.IO as TIO
import qualified GI.Cairo
import Data.GI.Gtk.Threading (postGUIASync)
import GI.Gdk (ModifierType(ModifierTypeButton1Mask), EventScroll)
import qualified GI.Gdk
import GI.Gtk hiding (FileChooserAction, Settings)
import qualified GI.Gtk  -- for FileChooserAction
import Data.GI.Base.Attributes
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Lens.Micro hiding (set)
import Lens.Micro.Mtl
import System.FilePath ((</>), (-<.>), takeBaseName)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Process (callCommand)

import Control.Monad.App.Class
import Settings
import ShowDialog
import qualified Types as T
import qualified Types.Application as T
import Paths_cabasa

-- | A concrete implementation of the 'MonadApp' interface.
newtype App a = App { getApp :: ReaderT T.Application IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader T.Application)

-- | Given a 'T.Application' state, run an 'App' in 'IO'.
runApp :: App a -> T.Application -> IO a
runApp = runReaderT . getApp

-- | Read an 'IORef' from 'T.Application' in 'App', using a 'Lens''.
readIORef' :: Lens' T.Application (IORef a) -> App a
readIORef' l = view l >>= liftIO . readIORef

-- | Write to an 'IORef' from 'T.Application' in 'App', using a 'Lens''.
writeIORef' :: Lens' T.Application (IORef a) -> a -> App ()
writeIORef' l v = view l >>= \r -> liftIO $ writeIORef r v

-- | Modify an 'IORef' from 'T.Application' in 'App', using a 'Lens''.
modifyIORefA' :: Lens' T.Application (IORef a) -> (a -> a) -> App ()
modifyIORefA' l f = view l >>= \r -> liftIO $ modifyIORef' r f

-- | Convert an 'IO' computation using 'T.Application' into an 'App'.
withApp :: (T.Application -> IO a) -> App a
withApp = App . ReaderT

modifyState' :: (forall n. T.ExistState' n -> T.ExistState' n) -> App ()
modifyState' f = App $ ReaderT $ \app -> T.modifyState app f

-- | Redraw the canvas. Used internally in the implementation of
-- @instance MonadApp app@.
redrawCanvas :: App ()
redrawCanvas = view T.canvas >>= liftIO . postGUIASync . widgetQueueDraw

-- | Set the icon of the ‘play\/pause button’ to ‘play’. Used
-- internally in the implementation of @instance MonadApp app@.
setPlayBtnIcon :: App ()
setPlayBtnIcon = view T.runIcon >>= \r -> liftIO $ imageSetFromStock r "gtk-media-play" $ param IconSizeButton

-- | Set the icon of the ‘play\/pause button’ to ‘pause’. Used
-- internally in the implementation of @instance MonadApp app@.
setPauseBtnIcon :: App ()
setPauseBtnIcon = view T.runIcon >>= \r -> liftIO $ imageSetFromStock r "gtk-media-pause" $ param IconSizeButton

-- | Convenience function to convert a 'FileChooserAction' (with a
-- phantom type parameter, for use with 'Optional') to the
-- corresponding 'GI.Gtk.FileChooserAction'
toGtkAction :: FileChooserAction i -> GI.Gtk.FileChooserAction
toGtkAction OpenFile = FileChooserActionOpen
toGtkAction SaveFile = FileChooserActionSave

data GtkMouseEvent =
    forall ev i1 i2 i3.
    ( AttrGetC i1 ev "state" [ModifierType]
    , AttrGetC i2 ev "x" Double
    , AttrGetC i3 ev "y" Double
    ) => GtkMouseEvent ev

instance Settings App where
    saveSettings ss = do
        writeIORef' T.settings ss
        liftIO $ settingsLocation >>= writeSettings ss
    getSetting s = view T.settings >>= liftIO . getSettingFrom s

instance GetOps App where
    getOps = do
        sRef <- view T.existState
        (T.ExistState (s :: T.ExistState' n)) <- liftIO $ readIORef sRef
        return Ops
            { getPattern = s ^. (T.currentPattern . _1)
            , getRule = s ^. T.rule
            , modifyPattern = \r -> do
                let (u, g) = s ^. T.currentPattern
                liftIO $ writeIORef sRef $ T.ExistState $ s & T.currentPattern .~ r u g
                redrawCanvas
            , getClipboard = s ^. T.clipboardContents
            , setClipboard = \u ->
                liftIO $ writeIORef sRef $ T.ExistState $ s & T.clipboardContents .~ u
            , defaultVal = s ^. T.defaultVal
            , defaultPattern = s ^. T.defaultPattern
            , states = F.finites
            , encodeInt = fromIntegral
            , decodeInt = F.finite . min (natVal $ Proxy @n) . toInteger
            , state2color = s ^. T.state2color
            , setState2Color = \u ->
                liftIO $ writeIORef sRef $ T.ExistState $ s & T.state2color .~ u
            , getName = s ^. T.getName
            }

instance Windows App where
    mainQuit = liftIO GI.Gtk.mainQuit
    setRuleWindowDelete = view T.setRuleWindow >>= liftIO . widgetHide
    stylesheetWindowDelete = view T.editSheetWindow >>= liftIO . widgetHide

    runGridSizeDialog (cols, rows) callback = do
        colsAdj <- view T.newNumColsAdjustment
        rowsAdj <- view T.newNumRowsAdjustment
        liftIO $ adjustmentSetValue colsAdj $ fromIntegral cols
        liftIO $ adjustmentSetValue rowsAdj $ fromIntegral rows   
        d <- view T.newGridSizeDialog
        dialogRun' d >>= \case
            AnotherResponseType 1 -> do  -- OK button
                newCols <- floor <$> adjustmentGetValue colsAdj
                newRows <- floor <$> adjustmentGetValue rowsAdj
                callback (Coord newCols) (Coord newRows)
            _ -> pure ()
        widgetHide d
    showSettingsDialog = do
        _ <- getSetting' T.predefinedRulesDir >>= \ss ->
            view T.predefRulesDirChooser >>= liftIO . flip fileChooserSetCurrentFolder ss

        _ <- getSetting' T.userRulesDir >>= \ss ->
            view T.userRulesDirChooser >>= liftIO . flip fileChooserSetCurrentFolder ss

        _ <- getSetting (T.gridSize . _Just . _1) >>= \ss ->
            view T.numColsAdjustment >>= liftIO . flip adjustmentSetValue (fromIntegral ss)

        _ <- getSetting (T.gridSize . _Just . _2) >>= \ss ->
            view T.numRowsAdjustment >>= liftIO . flip  adjustmentSetValue (fromIntegral ss)

        view T.settingsWindow >>= liftIO . dialogRun >>= \case
            1 -> do  -- OK button
                _predefinedRulesDir <- view T.predefRulesDirChooser >>= fileChooserGetFilename
                _userRulesDir       <- view T.userRulesDirChooser >>= fileChooserGetFilename

                _gridSize <- fmap Just $
                    (,) <$> (floor <$> (adjustmentGetValue =<< view T.numColsAdjustment))
                        <*> (floor <$> (adjustmentGetValue =<< view T.numRowsAdjustment))

                saveSettings (T.Settings {..})
            _ -> pure ()

        view T.settingsWindow >>= liftIO . widgetHide

        return ()

    showSetRuleWindow = ask >>= \app -> widgetShowAll (app ^. T.setRuleWindow)
    showEditSheetWindow = ask >>= \app -> widgetShowAll (app ^. T.editSheetWindow)

    showAboutDialog = view T.window >>= \w -> liftIO $ do
        a <- aboutDialogNew
        set a
            [ #transientFor := w
            , #programName  := "Cabasa"
            , #name         := "Cabasa"
            , #version      := "0.1"
            , #comments     := "An application for the simulation of arbitrary 2D cellular automata"
            , #authors      := ["Brad Neimann"]
            , #copyright    := "© Brad Neimann 2017-2018"
            ]
        _ <- dialogRun a
        widgetDestroy a

    showUserManual = liftIO $ do
        location <- getDataFileName "doc/UserManual.pdf"
#ifdef mingw32_HOST_OS
        callCommand $ "start " ++ location
#elif defined linux_HOST_OS
        callCommand $ "xdg-open" ++ location
#elif defined darwin_HOST_OS
        callCommand $ "open"     ++ location
#else
        showMessageDialog (Just window) MessageError ButtonsOk
            "ERROR: Could not figure out how to open manual on your system.\nThis is a bug - please report it on the project website" pure
#endif

    showErrorDialog msg = do
        w <- view T.window
        liftIO $ showMessageDialog w MessageTypeError ButtonsTypeOk msg $ const $ pure ()
    showQueryDialog msg no yes = do
        win <- view T.window
        app <- ask
        liftIO $ showMessageDialog win MessageTypeInfo ButtonsTypeYesNo msg $
            flip runApp app . \case { ResponseTypeYes -> yes ; _ -> no }

    withPatternFileDialog act callback = withApp $ \app -> do
        withFileDialogChoice (getPatternFileChooser app) (toGtkAction act) $ \_ path ->
            case act of
                SaveFile -> runApp (callback () path) app
                OpenFile -> do
                    p <- TIO.readFile path
                    runApp (callback p path) app
      where
        -- Returns a file chooser preconfigured to save or open pattern files
        getPatternFileChooser :: T.Application -> GI.Gtk.FileChooserAction -> IO FileChooserNative
        getPatternFileChooser app action = do
            fChooser <- fileChooserNativeNew
                Nothing
                (Just $ app ^. T.setRuleWindow)
                action
                Nothing Nothing

            mCellFilter <- fileFilterNew
            fileFilterSetName mCellFilter $ Just "MCell files (*.mcl)"
            fileFilterAddPattern mCellFilter "*.mcl"
            fileChooserAddFilter fChooser mCellFilter

            return fChooser


    withRuleFileDialog act callback = withApp $ \app -> do
        withFileDialogChoice (getRuleFileChooser app) (toGtkAction act) $ \_ fName -> do
            -- There are two places where the rule type can be
            -- specified: the extension of the chosen file, or the
            -- chosen file type filter. We use the former if it is
            -- present, otherwise use the latter.
            case act of
                SaveFile -> runApp (callback () fName) app
                OpenFile -> do
                    p <- TIO.readFile fName
                    runApp (callback p fName) app
      where
        -- Returns a file chooser preconfigured to save or open rule files
        getRuleFileChooser :: T.Application -> GI.Gtk.FileChooserAction -> IO FileChooserNative
        getRuleFileChooser app action = do
            fChooser <- fileChooserNativeNew
                Nothing
                (Just $ app ^. T.setRuleWindow)
                action
                Nothing Nothing

            alpacaFilter <- fileFilterNew
            fileFilterSetName alpacaFilter $ Just "ALPACA files"
            fileFilterAddPattern alpacaFilter "*.alp"
            fileChooserAddFilter fChooser alpacaFilter
            fileChooserSetFilter fChooser alpacaFilter

            return fChooser

    withCSSFileDialog act callback = withApp $ \app -> do
        withFileDialogChoice (getCSSFileChooser app) (toGtkAction act) $ \_ path -> do
            case act of
                SaveFile -> runApp (callback () path) app
                OpenFile -> do
                    p <- TIO.readFile path
                    runApp (callback p path) app
      where
        getCSSFileChooser :: T.Application -> GI.Gtk.FileChooserAction -> IO FileChooserNative
        getCSSFileChooser app ac = do
            fChooser <- fileChooserNativeNew
                Nothing
                (Just $ app ^. T.editSheetWindow)
                ac
                Nothing Nothing

            cssFilter <- fileFilterNew
            fileFilterSetName cssFilter $ Just "ALPACA Stylesheets files (*.css)"
            fileFilterAddPattern cssFilter "*.css"
            fileChooserAddFilter fChooser cssFilter

            return fChooser

instance Modes App where
    getCurrentMode = readIORef' T.currentMode
    setMode m = do
        writeIORef' T.currentMode m
        view T.drawopts >>= \w ->
            widgetSetSensitive w $ m == T.DrawMode
    setPastePending = modifyIORefA' T.currentMode T.PastePendingMode
    getCurrentDrawingState = ask >>= \app -> liftIO $
        comboBoxGetActiveIter (app ^. T.curstate) >>= \case
            (False, _) -> return 0
            (True, iter) -> treeModelGetValue (app ^. T.curstatem) iter 0 >>= (fmap fromIntegral . fromGValue @Int32)

instance PlayThread App where
    togglePlayThread preOnAct onAct =
        readIORef' T.runThread >>= \case
            Just t -> do
                liftIO $ killThread t
                writeIORef' T.runThread Nothing
                setPlayBtnIcon
            Nothing -> do
                _ <- preOnAct
                app <- ask
                t <- liftIO $ forkIO $ forever $ flip runApp app $ do
                    _ <- onAct
                    readIORef' T.delay >>= liftIO . threadDelay
                writeIORef' T.runThread (Just t)
                setPauseBtnIcon
    forceKillThread =
        readIORef' T.runThread >>= \case
            Nothing -> return ()
            Just t -> do
                liftIO $ killThread t
                writeIORef' T.runThread (Just t)
                setPlayBtnIcon

instance SaveRestorePattern App where
    saveRestorePattern = do
        p <- readIORef' T.pos
        modifyState' $ \state ->
            let T.ExistState'{T._currentPattern=(g, _), T._saved=s} = state
            in state & T.saved ?~ fromMaybe (g, p) s
    restorePattern = do
        app <- ask
        liftIO $ T.modifyStateM app $ \state ->
            case state ^. T.saved of
                Just prev -> do
                    writeIORef (app ^. T.pos) $ snd prev
                    return $ state & T.saved .~ Nothing
                                & (T.currentPattern . _1) .~ fst prev
                Nothing -> pure state
        redrawCanvas
    resetRestorePattern = modifyState' $ T.saved .~ Nothing

instance EvolutionSettings App where
    modifyGen f = withApp $ flip modifyGeneration f
    modifyDelay f = do
        d <- readIORef' T.delay
        let d' = f d
        writeIORef' T.delay d'
        view T.delayLbl >>= \l -> liftIO $ labelSetText l (pack $ show d')
    setCoordsLabel l = view T.coordsLbl >>= flip labelSetText l

instance Canvas App where
    getSelection = readIORef' T.selection
    setSelection s = writeIORef' T.selection s >> redrawCanvas

    getPos = readIORef' T.pos
    modifyPos f = modifyIORefA' T.pos f >> redrawCanvas

    modifyCellPos fCell fX fY = do
        ask >>= \app -> liftIO $
            modifyIORef (app ^. T.pos) $ over T.cellWidth  fCell
                                       . over T.cellHeight fCell
                                       . over T.leftXCoord fX
                                       . over T.topYCoord  fY
        redrawCanvas

    recordNewMousePoint p = do
        lastPoint <- readIORef' T.lastPoint
        writeIORef' T.lastPoint $ Just p
        return $ case lastPoint of
            Nothing -> NewPoint
            Just lp ->
                if lp == p
                then NoDiff
                else PointDiff (getDiff lp p)
      where
        getDiff (Point lastX lastY) (Point thisX thisY) =
            Point (thisX-lastX) (thisY-lastY)
    eraseMousePointRecord = writeIORef' T.lastPoint Nothing

    type MouseEvent App = GtkMouseEvent
    getMouseEventInfo (GtkMouseEvent ev) = liftIO $ do
        s <- get ev #state
        x <- get ev #x
        y <- get ev #y
        return (ModifierTypeButton1Mask `elem` s, (x, y))

    type ScrollEvent App = EventScroll
    getScrollEventInfo ev = liftIO $ do
        s <- get ev #direction >>= pure . \case
            GI.Gdk.ScrollDirectionUp -> ScrollDirectionUp
            GI.Gdk.ScrollDirectionDown -> ScrollDirectionDown
            _ -> ScrollDirectionOther
        x <- get ev #x
        y <- get ev #y
        return (s, (x, y))

    getPasteSelectionOverlay = readIORef' T.pasteSelectionOverlay
    setPasteSelectionOverlay o = do
        writeIORef' T.pasteSelectionOverlay o
        redrawCanvas

instance Paths App where
    getCurrentRuleName = withApp T.getCurrentRuleName
    getCurrentRulePath = readIORef' T.currentRulePath
    locateRuleByName rule cantFind = do
        predefDir <- getSetting' T.predefinedRulesDir
        userDir   <- getSetting' T.userRulesDir

        rules <- liftIO $ listDirectories [userDir, predefDir]
        let rulePathMay = find ((rule==) . takeBaseName) rules
        rulePath <- case rulePathMay of
            Nothing -> cantFind >>= \case
                Nothing -> return Nothing
                p@(Just _) -> return p
            p@(Just _) -> return p
        case rulePath of
            Nothing -> return Nothing
            Just path -> do
                contents <- liftIO $ TIO.readFile path
                return $ Just (path, contents)
      where
        listDirectories :: [FilePath] -> IO [FilePath]
        listDirectories ds =
            filterM doesDirectoryExist ds
            >>= traverse listDirectoryWithPath
            >>= (pure . concat)
          where
            listDirectoryWithPath dir = (fmap . fmap) (dir </>) $ listDirectory dir

    getRuleText = view T.newRuleBuf >>= \newRuleBuf -> liftIO $ do
        (start, end) <- textBufferGetBounds newRuleBuf
        textBufferGetText newRuleBuf start end True
    writeRule path rule = do
        liftIO $ TIO.writeFile path rule
        writeIORef' T.currentRulePath (Just path)
    setRuleWindowRule ruleText = withApp $ \app -> setTextBufferText (app ^. T.newRuleBuf) ruleText
    setCurrentRule path text = do
        gridSize <- getSetting' T.gridSize
        withApp $ \app -> case runALPACA text of
            Left err -> showMessageDialog (app ^. T.window)
                                        MessageTypeError
                                        ButtonsTypeOk
                                        (pack err)
                                        (const $ pure ())
            Right ad -> mkExistState app gridSize ad $ \(Proxy :: Proxy n) pressClear getExistState -> do
                g <- newStdGen
                T.withState app $ \old ->
                    let (oldPtn, _) = old ^. T.currentPattern
                    in writeIORef (app ^. T.existState) $ getExistState (finiteClamp <$> oldPtn) g
                modifyGeneration app (const 0)
                writeIORef (app ^. T.currentRulePath) path

                -- Update the ListStore with the new states
                let curstatem = app ^. T.curstatem
                    curstate  = app ^. T.curstate
                listStoreClear curstatem
                forM_ (F.finites @n) $ \val -> do
                    iter <- listStoreAppend curstatem
                    val' <- toGValue (fromIntegral val :: CInt)
                    listStoreSet curstatem iter [0] [val']
                comboBoxSetActive curstate 0

                when pressClear $ menuItemActivate (app ^. T.clearPattern)

                -- Because we're changing the currentPattern, we need to redraw
                widgetQueueDraw $ app ^. T.canvas
      where
        finiteClamp :: forall n m. (KnownNat n, KnownNat m) => F.Finite n -> F.Finite m
        finiteClamp = F.finite . min (natVal $ Proxy @m) . toInteger

        -- | Create a 'T.ExistState' given an 'AlpacaData'. This
        -- function has three outputs:
        --
        --     1. A 'Proxy' containing the number of states, as a
        --     type-level number
        --
        --     2. A 'Bool', stating if the screen needs to be cleared
        --     (as it may need to be if e.g. an ALPACA initial
        --     configuration has been defined and loaded into
        --     '_defaultPattern').
        --
        --     3. A function which, given a new value for the current
        --     universe and 'StdGen', will construct the new
        --     'T.ExistState'.
        --
        -- The main complication is that the three outputs need to be
        -- existentially quantified, and GHC doesn’t support returning
        -- a bare existential from a function, so in this case it is
        -- simpler to represent the outputs as
        -- '(forall n. o1 n -> o2 n -> a) -> a' rather than the
        -- equivalent yet non-existent 'exists n. (o1 n, o2 n)'.
        mkExistState :: T.Application
                     -> (Int, Int)
                     -> AlpacaData StdGen
                     -> ( forall n. KnownNat n
                        => Proxy n
                        -> Bool
                        -> (Universe (F.Finite n) -> StdGen -> T.ExistState)
                        -> a )
                     -> a
        mkExistState app
                     (numcols, numrows)
                     AlpacaData{ rule = (rule :: CARuleA (Rand StdGen) Point (F.Finite n'))
                               , initConfig, stateData }
                     f =
            f (Proxy @n') (isJust initConfig) $ \newUniv g -> T.ExistState $ T.ExistState'
                { _defaultSize = (Coord numcols, Coord numrows)
                , _defaultVal  = const 0
                , _state2color = \s -> (app ^. T.colors) !! fromInteger (F.getFinite s)
                , _rule = rule
                , _getName = Just . fst . stateData
                , _currentPattern = (newUniv, g)
                , _saved = Nothing
                , _clipboardContents = Nothing
                }

    getCurrentPatternPath = readIORef' T.currentPatternPath
    setCurrentPatternPath = writeIORef' T.currentPatternPath . Just
    writePattern path contents = do
        liftIO $ writeFile path contents
        writeIORef' T.currentPatternPath $ Just path

    getStylesheetText = view T.sheetBuf >>= \sheetBuf -> liftIO $ do
        (start, end) <- textBufferGetBounds sheetBuf
        textBufferGetText sheetBuf start end True
    getCurrentStylesheetPath = readIORef' T.currentStylesheetPath
    writeSheet file contents = view T.currentStylesheetPath >>= \path -> liftIO $ do
        TIO.writeFile (file -<.> "css") contents
        writeIORef path (Just file)
    setStylesheetWindowStylesheet ss = do
        sheetBuf <- view T.sheetBuf
        liftIO $ setTextBufferText sheetBuf $ pack ss

instance RenderCanvas App where
    type RenderContext App = GI.Cairo.Context
    -- from https://github.com/haskell-gi/haskell-gi/blob/36e4c4fb0df9e80d3c9b2f5999b65128e20317fb/examples/advanced/Cairo.hs#L297
    renderWithContext ct r = do
        canvas <- view T.canvas
        liftIO $ do
            size <- getCanvasSize canvas
            withManagedPtr ct $ \p -> runReaderT (runRender $ r size) (Cairo (castPtr p))
      where
        getCanvasSize canvas = do
            w <- fromIntegral <$> widgetGetAllocatedWidth canvas
            h <- fromIntegral <$> widgetGetAllocatedHeight canvas
            return (w, h)

modifyGeneration :: T.Application -> (Int -> Int) -> IO ()
modifyGeneration app f = do
    let generation    = app ^. T.generation
        generationLbl = app ^. T.generationLbl
    g <- readIORef generation
    let g' = f g
    writeIORef generation g'
    labelSetText generationLbl $ pack $ show g'

withFileDialogChoice :: (GI.Gtk.FileChooserAction -> IO FileChooserNative)
                     -> GI.Gtk.FileChooserAction
                     -> (FileChooserNative -> FilePath -> IO a)
                     -> IO (Maybe a)
withFileDialogChoice constr action contn = do
    fChooser <- constr action
    fmap (toEnum.fromIntegral) (#run fChooser) >>= \x ->
        if (x == ResponseTypeOk) || (x == ResponseTypeAccept) then
            fileChooserGetFilename fChooser >>= \case
                Just fName -> Just <$> contn fChooser fName
                Nothing -> pure Nothing
        else pure Nothing

-- Utilities for working with gi-gtk enums
-- Usage: like 'funcTakingAnEnum (param val)' or 'enum (funcReturningAnEnum)'

param :: Enum e => e -> Int32
param = fromIntegral . fromEnum

-- commented out as this isn't needed right now --- it's just giving
-- a 'defined but not used' warning.
-- uncomment if/when needed in the future
-- enum :: Enum e => Int32 -> e
-- enum = toEnum . fromIntegral
