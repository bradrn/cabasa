{-# LANGUAGE LambdaCase #-}

module ControlButtons (addControlButtonHandlers) where

import Control.Concurrent
import Control.Monad (forever)
import Data.IORef
import Data.Maybe (fromMaybe)

import CA (runRand, evolve)
import Data.Text (pack)
import Graphics.UI.Gtk
import Lens.Micro

import Common
import qualified Types as T

addControlButtonHandlers :: T.Application -> IO ()
addControlButtonHandlers app = do
    _ <- (app ^. T.step) `on` buttonActivated $ savePattern app >> runGen app postGUIAsync
    _ <- (app ^. T.run) `on` buttonActivated $ runButtonHandler app
    _ <- (app ^. T.reset) `on` buttonActivated $ resetButtonHandler app
    return ()

runButtonHandler :: T.Application -> IO ()
runButtonHandler app = readIORef (app ^. T.runThread) >>= \case
    Just t -> do
        killThread t
        writeIORef (app ^. T.runThread) Nothing
        imageSetFromStock (app ^. T.runIcon) (pack "gtk-media-play") IconSizeButton
    Nothing -> do
        savePattern app
        t <- forkIO $ forever $ runGen app postGUISync >> threadDelay 100000
        writeIORef (app ^. T.runThread) $ Just t
        imageSetFromStock (app ^. T.runIcon) (pack "gtk-media-pause") IconSizeButton

resetButtonHandler :: T.Application -> IO ()
resetButtonHandler app = do
    readIORef (app ^. T.runThread) >>= \case
        Just t -> do
            killThread t
            writeIORef (app ^. T.runThread) Nothing
            imageSetFromStock (app ^. T.runIcon) (pack "gtk-media-play") IconSizeButton
        Nothing -> return ()
    popPattern app
    widgetQueueDraw (app ^. T.canvas)

savePattern :: T.Application -> IO ()
savePattern app = do
    p <- readIORef $ app ^. T.pos
    T.modifyState app $ \state ->
        let T.ExistState'{T._currentPattern=(g, _), T._saved=s} = state
        in state & T.saved .~ Just (fromMaybe (g, p) s)

popPattern :: T.Application -> IO ()
popPattern app = do
    modifyGeneration app (const 0)
    T.modifyStateM app $ \state ->
        case state ^. T.saved of
            Just prev -> do
                writeIORef (app ^. T.pos) $ snd prev
                return $ state & T.saved .~ Nothing
                               & (T.currentPattern . _1) .~ fst prev
            Nothing -> pure state

-- When runGen is called from the main thread, we want to use
-- postGUIAsync, but when it's called from any other thread, we want to
-- use postGUISync - so we provide an argument to select the function.
-- See http://gtk2hs-users.narkive.com/QvCQw4q3/use-of-postguisync-within-the-main-gtk-thread
runGen :: T.Application -> (IO () -> IO ()) -> IO ()
runGen app postFn = do
    T.modifyState app $ \state ->
        let r = state ^. T.rule
            (g, s) = state ^. T.currentPattern
        in state & T.currentPattern .~ runRand (evolve r g) s
    modifyGeneration app (+1)
    postFn (widgetQueueDraw $ app ^. T.canvas)
