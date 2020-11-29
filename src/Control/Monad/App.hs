{-# OPTIONS_GHC -Werror=missing-fields -Werror=missing-methods #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Control.Monad.App
    ( App
    , runApp
    , GtkMouseEvent(..)
    ) where

import Control.Concurrent (killThread, forkIO, threadDelay)
import Data.Bifunctor (first)
import Data.Int (Int32)
import Data.IORef
import Data.Maybe (fromMaybe)
import Foreign.Ptr (castPtr)

import CA.Universe (Point(..), Coord(..))
import Control.Monad.Reader
import Data.Text (pack)

import qualified Data.Text.IO as TIO
import qualified GI.Cairo
import Data.GI.Gtk.Threading (postGUIASync)
import GI.Gdk (ModifierType(ModifierTypeButton1Mask), EventScroll)
import qualified GI.Gdk
import GI.Gtk hiding (FileChooserAction, Settings, Clipboard)
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
import qualified Cabasa.ShowDialog as SD
import qualified Cabasa.Types as T
import qualified Cabasa.Types.Application as T
import Paths_cabasa

-- | A concrete implementation of the 'MonadApp' interface.
newtype App a x = App { getApp :: ReaderT (T.Application a) IO x }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (T.Application a))

-- | Given a 'T.Application' state, run an 'App' in 'IO'.
runApp :: App a x -> T.Application a -> IO x
runApp = runReaderT . getApp

-- | Read an 'IORef' from 'T.Application' in 'App', using a 'Lens''.
readIORef' :: Lens' (T.Application a) (IORef x) -> App a x
readIORef' l = view l >>= liftIO . readIORef

-- | Write to an 'IORef' from 'T.Application' in 'App', using a 'Lens''.
writeIORef' :: Lens' (T.Application a) (IORef x) -> x -> App a ()
writeIORef' l v = view l >>= \r -> liftIO $ writeIORef r v

-- | Modify an 'IORef' from 'T.Application' in 'App', using a 'Lens''.
modifyIORefA' :: Lens' (T.Application a) (IORef x) -> (x -> x) -> App a ()
modifyIORefA' l f = view l >>= \r -> liftIO $ modifyIORef' r f

-- | Convert an 'IO' computation using 'T.Application' into an 'App'.
withApp :: (T.Application a -> IO x) -> App a x
withApp = App . ReaderT

-- | Redraw the canvas. Used internally in the implementation of
-- @instance MonadApp app@.
redrawCanvas :: App a ()
redrawCanvas = view T.canvas >>= liftIO . postGUIASync . widgetQueueDraw

-- | Set the icon of the ‘play\/pause button’ to ‘play’. Used
-- internally in the implementation of @instance MonadApp app@.
setPlayBtnIcon :: App a ()
setPlayBtnIcon = view T.runIcon >>= \r -> liftIO $ imageSetFromStock r "gtk-media-play" $ param IconSizeButton

-- | Set the icon of the ‘play\/pause button’ to ‘pause’. Used
-- internally in the implementation of @instance MonadApp app@.
setPauseBtnIcon :: App a ()
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

instance Pattern a (App a) where
    getPattern = fst <$> readIORef' T.currentPattern
    modifyPattern r = do
        modifyIORefA' T.currentPattern $ uncurry r
        redrawCanvas

instance HasRuleConfig a (App a) where
    askRuleConfig = view T.ruleConfig

    states = view T.states
    encodeInt = view T.encodeInt
    decodeInt = view T.decodeInt

instance Windows (App a) where
    showMessageDialog mt bt t c =
        withApp $ \app -> SD.showMessageDialog (app ^. T.window) mt bt t $ \r -> runApp (c r) app

    mainQuit = liftIO GI.Gtk.mainQuit

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

instance Modes (App a) where
    getCurrentMode = readIORef' T.currentMode
    setMode m = do
        writeIORef' T.currentMode m
        view T.drawopts >>= \w ->
            widgetSetSensitive w $ m == T.DrawMode
    getCurrentDrawingState = ask >>= \app -> liftIO $
        comboBoxGetActiveIter (app ^. T.curstate) >>= \case
            (False, _) -> return 0
            (True, iter) -> treeModelGetValue (app ^. T.curstatem) iter 0 >>= (fmap fromIntegral . fromGValue @Int32)

instance PlayThread (App a) where
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

instance SaveRestorePattern (App a) where
    saveRestorePattern = do
        p <- readIORef' T.pos
        s <- readIORef' T.saved
        (g, _) <- readIORef' T.currentPattern
        writeIORef' T.saved $ Just $ fromMaybe (g, p) s
    restorePattern = do
        app <- ask
        liftIO $ do
            readIORef (app ^. T.saved) >>= \case
                Just prev -> do
                    writeIORef (app ^. T.pos) $ snd prev
                    writeIORef (app ^. T.saved) Nothing
                    modifyIORef (app ^. T.currentPattern) $ first $ const $ fst prev
                Nothing -> pure ()
        redrawCanvas
    resetRestorePattern = writeIORef' T.saved Nothing

instance EvolutionSettings (App a) where
    modifyGen f = withApp $ flip modifyGeneration f
    setCoordsLabel l = view T.coordsLbl >>= flip labelSetText l

instance Canvas (App a) where
    getSelection = readIORef' T.selection
    setSelection s = writeIORef' T.selection s >> redrawCanvas

    getPos = readIORef' T.pos
    modifyPos f = modifyIORefA' T.pos f >> redrawCanvas

    getColors = asks T._colors

    type MouseEvent (App a) = GtkMouseEvent
    getMouseEventInfo (GtkMouseEvent ev) = liftIO $ do
        s <- get ev #state
        x <- get ev #x
        y <- get ev #y
        return (ModifierTypeButton1Mask `elem` s, (x, y))

    type ScrollEvent (App a) = EventScroll
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

instance MouseTracking (App a) where
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

instance Files (App a) where
    listDirectories ds = liftIO $
        filterM doesDirectoryExist ds
        >>= traverse listDirectoryWithPath
        >>= (pure . concat)
        where
        listDirectoryWithPath dir = (fmap . fmap) (dir </>) $ listDirectory dir
    readTextFile = liftIO . TIO.readFile

instance Paths (App a) where
    getCurrentRuleName = withApp T.getCurrentRuleName

    getCurrentPatternPath = readIORef' T.currentPatternPath
    setCurrentPatternPath = writeIORef' T.currentPatternPath . Just
    writePattern path contents = do
        liftIO $ writeFile path contents
        writeIORef' T.currentPatternPath $ Just path

instance RenderCanvas (App a) where
    type RenderContext (App a) = GI.Cairo.Context
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

instance Clipboard a (App a) where
    getClipboard = readIORef' T.clipboardContents
    setClipboard u = writeIORef' T.clipboardContents u

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
