{-# OPTIONS_GHC -Werror=missing-fields -Werror=missing-methods #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Control.Monad.App
    ( App
    , runApp
    , GtkMouseEvent(..)
    ) where

import Control.Applicative ((<|>))
import Control.Concurrent (killThread, forkIO, threadDelay)
import Data.Int (Int32)
import Data.IORef
import Data.List (find)
import Data.Maybe (fromMaybe)
import Foreign.Ptr (castPtr)

import Control.Monad.Reader
import Data.Text (pack, uncons)
import qualified Data.Text.IO as TIO
import qualified GI.Cairo
import Data.GI.Gtk.Threading (postGUIASync)
import GI.Gdk (ModifierType(ModifierTypeButton1Mask), EventScroll)
import qualified GI.Gdk
import GI.Gtk hiding (FileChooserAction)
import qualified GI.Gtk  -- for FileChooserAction
import Data.GI.Base.Attributes
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Lens.Micro hiding (set)
import System.FilePath ((</>), (-<.>), takeBaseName, takeExtension)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Process (callCommand)

import CA.Universe (Point(..), Coord(..))
import Control.Monad.App.Class
import qualified Utils as U
import Settings (getSetting')
import qualified SettingsDialog as S
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
readIORef' l = asks (^. l) >>= liftIO . readIORef

-- | Write to an 'IORef' from 'T.Application' in 'App', using a 'Lens''.
writeIORef' :: Lens' T.Application (IORef a) -> a -> App ()
writeIORef' l v = asks (^. l) >>= \r -> liftIO $ writeIORef r v

-- | Modify an 'IORef' from 'T.Application' in 'App', using a 'Lens''.
modifyIORefA' :: Lens' T.Application (IORef a) -> (a -> a) -> App ()
modifyIORefA' l f = asks (^. l) >>= \r -> liftIO $ modifyIORef' r f

-- | Convert an 'IO' computation using 'T.Application' into an 'App'.
withApp :: (T.Application -> IO a) -> App a
withApp = App . ReaderT

-- | Redraw the canvas. Used internally in the implementation of
-- @instance MonadApp app@.
redrawCanvas :: App ()
redrawCanvas = asks (^. T.canvas) >>= liftIO . postGUIASync . widgetQueueDraw

-- | Set the icon of the ‘play\/pause button’ to ‘play’. Used
-- internally in the implementation of @instance MonadApp app@.
setPlayBtnIcon :: App ()
setPlayBtnIcon = withApp $ \app -> imageSetFromStock (app ^. T.runIcon) (pack "gtk-media-play")  $ U.param IconSizeButton

-- | Set the icon of the ‘play\/pause button’ to ‘pause’. Used
-- internally in the implementation of @instance MonadApp app@.
setPauseBtnIcon :: App ()
setPauseBtnIcon = withApp $ \app -> imageSetFromStock (app ^. T.runIcon) (pack "gtk-media-pause") $ U.param IconSizeButton

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


instance MonadApp App where
    getOps = do
        sRef <- asks (^. T.existState)
        (T.ExistState s) <- liftIO $ readIORef sRef
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
            , states = s ^. T.states
            , encodeInt = s ^. T.encodeInt
            , decodeInt = s ^. T.decodeInt
            , state2color = s ^. T.state2color
            , setState2Color = \u ->
                liftIO $ writeIORef sRef $ T.ExistState $ s & T.state2color .~ u
            , getName = s ^. T.getName
            }

    mainQuit = liftIO GI.Gtk.mainQuit
    setRuleWindowDelete = asks (^. T.setRuleWindow) >>= liftIO . widgetHide
    stylesheetWindowDelete = asks (^. T.editSheetWindow) >>= liftIO . widgetHide

    getCurrentMode = readIORef' T.currentMode
    setMode m = do
        writeIORef' T.currentMode m
        asks (^. T.drawopts) >>= \w ->
            widgetSetSensitive w $ m == T.DrawMode
    setPastePending = modifyIORefA' T.currentMode T.PastePendingMode
    getCurrentDrawingState = ask >>= \app -> liftIO $
        comboBoxGetActiveIter (app ^. T.curstate) >>= \case
            (False, _) -> return 0
            (True, iter) -> treeModelGetValue (app ^. T.curstatem) iter 0 >>= (fmap fromIntegral . fromGValue @Int32)

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

    saveRestorePattern = withApp $ \app -> do
        p <- readIORef $ app ^. T.pos
        T.modifyState app $ \state ->
            let T.ExistState'{T._currentPattern=(g, _), T._saved=s} = state
            in state & T.saved .~ Just (fromMaybe (g, p) s)
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
    resetRestorePattern = withApp $ \app ->
        T.modifyState app $ T.saved .~ Nothing

    modifyGen f = withApp $ flip U.modifyGeneration f
    modifyDelay f = do
        old <- readIORef' T.delay
        let new = f old
        writeIORef' T.delay new
        asks (^. T.delayLbl) >>= \l -> liftIO $ labelSetText l (pack $ show new)
    setCoordsLabel l = asks (^. T.coordsLbl) >>= flip labelSetText l

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

    runGridSizeDialog (cols, rows) callback = do
        colsAdj <- asks (^. T.newNumColsAdjustment)
        rowsAdj <- asks (^. T.newNumRowsAdjustment)
        liftIO $ adjustmentSetValue colsAdj $ fromIntegral cols
        liftIO $ adjustmentSetValue rowsAdj $ fromIntegral rows   
        d <- asks (^. T.newGridSizeDialog)
        U.dialogRun' d >>= \case
            AnotherResponseType 1 -> do  -- OK button
                newCols <- floor <$> adjustmentGetValue colsAdj
                newRows <- floor <$> adjustmentGetValue rowsAdj
                callback (Coord newCols) (Coord newRows)
            _ -> pure ()
        widgetHide d
    showSettingsDialog = withApp S.showSettingsDialog

    showSetRuleWindow = ask >>= \app -> widgetShowAll (app ^. T.setRuleWindow)
    showEditSheetWindow = ask >>= \app -> widgetShowAll (app ^. T.editSheetWindow)

    showAboutDialog = withApp $ \app -> do
        a <- aboutDialogNew
        set a
            [ #transientFor := app ^. T.window
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
        w <- asks (^. T.window)
        liftIO $ U.showMessageDialog w MessageTypeError ButtonsTypeOk msg $ const $ pure ()
    showQueryDialog msg no yes = withApp $ \app -> do
        U.showMessageDialog (app ^. T.window) MessageTypeInfo ButtonsTypeYesNo msg
            $ flip runApp app . \case { ResponseTypeYes -> yes ; _ -> no }

    withPatternFileDialog act callback = withApp $ \app -> do
        U.withFileDialogChoice (U.getPatternFileChooser app) (toGtkAction act) $ \_ path ->
            case act of
                SaveFile -> runApp (callback () path) app
                OpenFile -> do
                    p <- TIO.readFile path
                    runApp (callback p path) app
    withRuleFileDialog act filterType callback = withApp $ \app -> do
        U.withFileDialogChoice (U.getRuleFileChooser app filterType) (toGtkAction act) $ \fChooser fName -> do
            -- There are two places where the rule type can be
            -- specified: the extension of the chosen file, or the
            -- chosen file type filter. We use the former if it is
            -- present, otherwise use the latter.
            let ruleTypeExt = case takeExtension fName of
                    ".alp" -> Just T.ALPACA
                    ".hs"  -> Just T.Hint
                    ".lhs" -> Just T.Hint
                    _      -> Nothing
            ruleTypeFilter <- fileChooserGetFilter fChooser >>= \case
                Just fFilter -> fileFilterGetName fFilter <&> \case
                    -- As we know that there are only two filters, the first
                    -- character of the filter offers a useful heuristic to
                    -- determine the file type
                    Just (uncons -> Just (x, _)) -> case x of
                        'A' -> Just T.ALPACA
                        'H' -> Just T.Hint
                        _   -> Nothing
                    _ -> Nothing
                Nothing -> return Nothing
            let ruleType = ruleTypeExt <|> ruleTypeFilter
            case act of
                SaveFile -> runApp (callback ruleType () fName) app
                OpenFile -> do
                    p <- TIO.readFile fName
                    runApp (callback ruleType p fName) app
    withCSSFileDialog act callback = withApp $ \app -> do
        U.withFileDialogChoice (getCSSFileChooser app) (toGtkAction act) $ \_ path -> do
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

    getCurrentRuleName = withApp T.getCurrentRuleName
    getCurrentRulePath = readIORef' T.currentRulePath
    locateRuleByName rule cantFind = withApp $ \app -> do
        predefDir <- getSetting' T.predefinedRulesDir app
        userDir   <- getSetting' T.userRulesDir       app

        rules <- listDirectories [userDir, predefDir]
        let rulePathMay = find ((rule==) . takeBaseName) rules
        rulePath <- case rulePathMay of
            Nothing -> runApp cantFind app >>= \case
                Nothing -> return Nothing
                p@(Just _) -> return p
            p@(Just _) -> return p
        case rulePath of
            Nothing -> return Nothing
            Just path -> do
                contents <- TIO.readFile path
                return $ Just (path, contents)
      where
        listDirectories :: [FilePath] -> IO [FilePath]
        listDirectories ds =
            filterM doesDirectoryExist ds
            >>= traverse listDirectoryWithPath
            >>= (pure . concat)
          where
            listDirectoryWithPath dir = (fmap . fmap) (dir </>) $ listDirectory dir

    getRuleText = asks (^. T.newRuleBuf) >>= \newRuleBuf -> liftIO $ do
        (start, end) <- textBufferGetBounds newRuleBuf
        textBufferGetText newRuleBuf start end True
    getCurrentLang = withApp $ \app -> do
        alpacaOn <- checkMenuItemGetActive (app ^. T.alpacaLang)
        haskellOn <- checkMenuItemGetActive (app ^. T.haskellLang)
        return $ if | alpacaOn  -> T.ALPACA
                    | haskellOn -> T.Hint
                    | otherwise -> error "Error in radio button at getCurrentLang!\nThis is a bug; please report it to the package maintainer."
    writeRule path rule = do
        liftIO $ TIO.writeFile path rule
        writeIORef' T.currentRulePath (Just path)
    setRuleWindowRule ruleText ruleType = withApp $ \app -> do
        setTextBufferText (app ^. T.newRuleBuf) ruleText
        case ruleType of
            T.ALPACA -> checkMenuItemSetActive (app ^. T.alpacaLang)  True
            T.Hint   -> checkMenuItemSetActive (app ^. T.haskellLang) True
    setCurrentRule file contents ruleType = withApp $ \app -> U.setCurrentRule app file contents ruleType

    getCurrentPatternPath = readIORef' T.currentPatternPath
    setCurrentPatternPath = writeIORef' T.currentPatternPath . Just
    writePattern path contents = do
        liftIO $ writeFile path contents
        writeIORef' T.currentPatternPath $ Just path
    
    getStylesheetText = asks (^. T.sheetBuf) >>= \sheetBuf -> liftIO $ do
        (start, end) <- textBufferGetBounds sheetBuf
        textBufferGetText sheetBuf start end True
    getCurrentStylesheetPath = readIORef' T.currentStylesheetPath
    writeSheet file contents = asks (^. T.currentStylesheetPath) >>= \path -> liftIO $ do
        TIO.writeFile (file -<.> "css") contents
        writeIORef path (Just file)
    setStylesheetWindowStylesheet ss = do
        sheetBuf <- asks (^. T.sheetBuf)
        liftIO $ setTextBufferText sheetBuf $ pack ss

    type RenderContext App = GI.Cairo.Context
    -- from https://github.com/haskell-gi/haskell-gi/blob/36e4c4fb0df9e80d3c9b2f5999b65128e20317fb/examples/advanced/Cairo.hs#L297
    renderWithContext ct r = do
        canvas <- asks (^. T.canvas)
        liftIO $ do
            size <- getCanvasSize canvas
            withManagedPtr ct $ \p -> runReaderT (runRender $ r size) (Cairo (castPtr p))
      where
        getCanvasSize canvas = do
            w <- fromIntegral <$> widgetGetAllocatedWidth canvas
            h <- fromIntegral <$> widgetGetAllocatedHeight canvas
            return (w, h)
