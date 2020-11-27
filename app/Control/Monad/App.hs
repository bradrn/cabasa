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
import Data.Maybe (fromMaybe)
import Data.Proxy
import Foreign.C.Types (CInt)
import Foreign.Ptr (castPtr)
import GHC.TypeLits (natVal, KnownNat)

import CA.Universe (Point(..), Coord(..))
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
import System.FilePath ((</>), (-<.>))
import System.Directory (doesDirectoryExist, listDirectory)
import System.Process (callCommand)

import Control.Monad.App.Class
import Settings
import qualified ShowDialog as SD
import qualified Types as T
import qualified Types.Application as T
import Paths_cabasa

-- | A concrete implementation of the 'MonadApp' interface.
newtype App n a = App { getApp :: ReaderT (T.Application n) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (T.Application n))

-- | Given a 'T.Application' state, run an 'App' in 'IO'.
runApp :: App n a -> T.Application n -> IO a
runApp = runReaderT . getApp

-- | Read an 'IORef' from 'T.Application' in 'App', using a 'Lens''.
readIORef' :: Lens' (T.Application n) (IORef a) -> App n a
readIORef' l = view l >>= liftIO . readIORef

-- | Write to an 'IORef' from 'T.Application' in 'App', using a 'Lens''.
writeIORef' :: Lens' (T.Application n) (IORef a) -> a -> App n ()
writeIORef' l v = view l >>= \r -> liftIO $ writeIORef r v

-- | Modify an 'IORef' from 'T.Application' in 'App', using a 'Lens''.
modifyIORefA' :: Lens' (T.Application n) (IORef a) -> (a -> a) -> App n ()
modifyIORefA' l f = view l >>= \r -> liftIO $ modifyIORef' r f

-- | Convert an 'IO' computation using 'T.Application' into an 'App'.
withApp :: (T.Application n -> IO a) -> App n a
withApp = App . ReaderT

-- | Redraw the canvas. Used internally in the implementation of
-- @instance MonadApp app@.
redrawCanvas :: App n ()
redrawCanvas = view T.canvas >>= liftIO . postGUIASync . widgetQueueDraw

-- | Set the icon of the ‘play\/pause button’ to ‘play’. Used
-- internally in the implementation of @instance MonadApp app@.
setPlayBtnIcon :: App n ()
setPlayBtnIcon = view T.runIcon >>= \r -> liftIO $ imageSetFromStock r "gtk-media-play" $ param IconSizeButton

-- | Set the icon of the ‘play\/pause button’ to ‘pause’. Used
-- internally in the implementation of @instance MonadApp app@.
setPauseBtnIcon :: App n ()
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

instance Settings (App n) where
    saveSettings ss = do
        writeIORef' T.settings ss
        liftIO $ settingsLocation >>= writeSettings ss
    getSetting s = view T.settings >>= liftIO . getSettingFrom s

instance KnownNat n => GetOps (F.Finite n) (App n) where
    getOps = do
        sRef <- view T.existState
        (s :: T.ExistState n) <- liftIO $ readIORef sRef
        return Ops
            { getPattern = s ^. (T.currentPattern . _1)
            , getRule = s ^. T.rule
            , modifyPattern = \r -> do
                let (u, g) = s ^. T.currentPattern
                liftIO $ writeIORef sRef $ s & T.currentPattern .~ r u g
                redrawCanvas
            , getClipboard = s ^. T.clipboardContents
            , setClipboard = \u ->
                liftIO $ writeIORef sRef $ s & T.clipboardContents .~ u
            , defaultVal = s ^. T.defaultVal
            , defaultPattern = s ^. T.defaultPattern
            , states = F.finites
            , encodeInt = fromIntegral
            , decodeInt = F.finite . min (natVal $ Proxy @n) . toInteger
            , state2color = s ^. T.state2color
            , setState2Color = \u ->
                liftIO $ writeIORef sRef $ s & T.state2color .~ u
            , getName = s ^. T.getName
            }

instance Windows (App n) where
    showMessageDialog mt bt t c =
        withApp $ \app -> SD.showMessageDialog (app ^. T.window) mt bt t $ \r -> runApp (c r) app

    mainQuit = liftIO GI.Gtk.mainQuit
    stylesheetWindowDelete = view T.editSheetWindow >>= liftIO . widgetHide

    runGridSizeDialog (cols, rows) callback = do
        colsAdj <- view T.newNumColsAdjustment
        rowsAdj <- view T.newNumRowsAdjustment
        liftIO $ adjustmentSetValue colsAdj $ fromIntegral cols
        liftIO $ adjustmentSetValue rowsAdj $ fromIntegral rows   
        d <- view T.newGridSizeDialog
        SD.dialogRun' d >>= \case
            AnotherResponseType 1 -> do  -- OK button
                newCols <- floor <$> adjustmentGetValue colsAdj
                newRows <- floor <$> adjustmentGetValue rowsAdj
                callback (Coord newCols) (Coord newRows)
            _ -> pure ()
        widgetHide d
    showSettingsDialog callback = do
        _ <- getSetting (T.gridSize . _Just . _1) >>= \ss ->
            view T.numColsAdjustment >>= liftIO . flip adjustmentSetValue (fromIntegral ss)

        _ <- getSetting (T.gridSize . _Just . _2) >>= \ss ->
            view T.numRowsAdjustment >>= liftIO . flip  adjustmentSetValue (fromIntegral ss)

        view T.settingsWindow >>= liftIO . dialogRun >>= \case
            1 -> do  -- OK button
                _gridSize <- fmap Just $
                    (,) <$> (floor <$> (adjustmentGetValue =<< view T.numColsAdjustment))
                        <*> (floor <$> (adjustmentGetValue =<< view T.numRowsAdjustment))

                callback (T.Settings {..})
            _ -> pure ()

        view T.settingsWindow >>= liftIO . widgetHide

        return ()

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
        SD.showMessageDialog (Just window) MessageError ButtonsOk
            "ERROR: Could not figure out how to open manual on your system.\nThis is a bug - please report it on the project website" pure
#endif

    showErrorDialog msg = do
        w <- view T.window
        liftIO $ SD.showMessageDialog w MessageTypeError ButtonsTypeOk msg $ const $ pure ()
    showQueryDialog msg no yes = do
        win <- view T.window
        app <- ask
        liftIO $ SD.showMessageDialog win MessageTypeInfo ButtonsTypeYesNo msg $
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
        getPatternFileChooser :: T.Application n -> GI.Gtk.FileChooserAction -> IO FileChooserNative
        getPatternFileChooser app action = do
            fChooser <- fileChooserNativeNew
                Nothing
                (Just $ app ^. T.window)
                action
                Nothing Nothing

            mCellFilter <- fileFilterNew
            fileFilterSetName mCellFilter $ Just "MCell files (*.mcl)"
            fileFilterAddPattern mCellFilter "*.mcl"
            fileChooserAddFilter fChooser mCellFilter

            return fChooser


    withCSSFileDialog act callback = withApp $ \app -> do
        withFileDialogChoice (getCSSFileChooser app) (toGtkAction act) $ \_ path -> do
            case act of
                SaveFile -> runApp (callback () path) app
                OpenFile -> do
                    p <- TIO.readFile path
                    runApp (callback p path) app
      where
        getCSSFileChooser :: T.Application n -> GI.Gtk.FileChooserAction -> IO FileChooserNative
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

instance Modes (App n) where
    getCurrentMode = readIORef' T.currentMode
    setMode m = do
        writeIORef' T.currentMode m
        view T.drawopts >>= \w ->
            widgetSetSensitive w $ m == T.DrawMode
    getCurrentDrawingState = ask >>= \app -> liftIO $
        comboBoxGetActiveIter (app ^. T.curstate) >>= \case
            (False, _) -> return 0
            (True, iter) -> treeModelGetValue (app ^. T.curstatem) iter 0 >>= (fmap fromIntegral . fromGValue @Int32)

instance PlayThread (App n) where
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
                writeIORef' T.runThread Nothing
                setPlayBtnIcon
    modifyDelay f = do
        d <- readIORef' T.delay
        let d' = f d
        writeIORef' T.delay d'
        view T.delayLbl >>= \l -> liftIO $ labelSetText l (pack $ show d')

instance SaveRestorePattern (App n) where
    saveRestorePattern = do
        p <- readIORef' T.pos
        modifyIORefA' T.existState $ \state@T.ExistState{T._currentPattern=(g, _), T._saved=s} ->
            state & T.saved ?~ fromMaybe (g, p) s
    restorePattern = do
        app <- ask
        liftIO $ do
            state <- readIORef $ app ^. T.existState
            state' <- case state ^. T.saved of
                Just prev -> do
                    writeIORef (app ^. T.pos) $ snd prev
                    return $ state & T.saved .~ Nothing
                                & (T.currentPattern . _1) .~ fst prev
                Nothing -> pure state
            writeIORef (app ^. T.existState) state'
        redrawCanvas
    resetRestorePattern = modifyIORefA' T.existState $ T.saved .~ Nothing

instance EvolutionSettings (App n) where
    modifyGen f = withApp $ flip modifyGeneration f
    setCoordsLabel l = view T.coordsLbl >>= flip labelSetText l

instance Canvas (App n) where
    getSelection = readIORef' T.selection
    setSelection s = writeIORef' T.selection s >> redrawCanvas

    getPos = readIORef' T.pos
    modifyPos f = modifyIORefA' T.pos f >> redrawCanvas

    getColors = asks T._colors

    type MouseEvent (App n) = GtkMouseEvent
    getMouseEventInfo (GtkMouseEvent ev) = liftIO $ do
        s <- get ev #state
        x <- get ev #x
        y <- get ev #y
        return (ModifierTypeButton1Mask `elem` s, (x, y))

    type ScrollEvent (App n) = EventScroll
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

    forceCanvasRedraw = redrawCanvas

instance MouseTracking (App n) where
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

instance Files (App n) where
    listDirectories ds = liftIO $
        filterM doesDirectoryExist ds
        >>= traverse listDirectoryWithPath
        >>= (pure . concat)
        where
        listDirectoryWithPath dir = (fmap . fmap) (dir </>) $ listDirectory dir
    readTextFile = liftIO . TIO.readFile

instance Paths (App n) where
    getCurrentRuleName = withApp T.getCurrentRuleName

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

instance RenderCanvas (App n) where
    type RenderContext (App n) = GI.Cairo.Context
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

modifyGeneration :: T.Application n -> (Int -> Int) -> IO ()
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
