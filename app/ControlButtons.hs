{-# LANGUAGE LambdaCase #-}

module ControlButtons where

import Control.Concurrent
import Control.Monad (forever)
import Data.IORef
import Data.Maybe (fromMaybe)

import CA (runRand, evolve)
import Data.Text (pack)
import Graphics.UI.Gtk
import Lens.Micro

import Common
import Types

addControlButtonHandlers :: Application -> IO ()
addControlButtonHandlers app = do
    _ <- (app ^. step) `on` buttonActivated $ savePattern app >> runGen app postGUIAsync
    _ <- (app ^. run) `on` buttonActivated $ runButtonHandler app
    _ <- (app ^. reset) `on` buttonActivated $ resetButtonHandler app
    return ()

runButtonHandler :: Application -> IO ()
runButtonHandler app = readIORef (app ^. runThread) >>= \case
    Just t -> do
        killThread t
        writeIORef (app ^. runThread) Nothing
        imageSetFromStock (app ^. runIcon) (pack "gtk-media-play") IconSizeButton
    Nothing -> do
        savePattern app
        t <- forkIO $ forever $ runGen app postGUISync >> threadDelay 100000
        writeIORef (app ^. runThread) $ Just t
        imageSetFromStock (app ^. runIcon) (pack "gtk-media-pause") IconSizeButton

resetButtonHandler :: Application -> IO ()
resetButtonHandler app = do
    readIORef (app ^. runThread) >>= \case
        Just t -> do
            killThread t
            writeIORef (app ^. runThread) Nothing
            imageSetFromStock (app ^. runIcon) (pack "gtk-media-play") IconSizeButton
        Nothing -> return ()
    popPattern app
    widgetQueueDraw (app ^. canvas)

savePattern :: Application -> IO ()
savePattern app = do
    p <- readIORef $ app ^. pos
    modifyState app $ \state ->
        let ExistState'{_currentPattern=(g, _), _saved=s} = state
        in state & saved .~ Just (fromMaybe (g, p) s)

popPattern :: Application -> IO ()
popPattern app = do
    modifyGeneration app (const 0)
    modifyStateM app $ \state ->
        case state ^. saved of
            Just prev -> do
                writeIORef (app ^. pos) $ snd prev
                return $ state & saved .~ Nothing
                               & (currentPattern . _1) .~ fst prev
            Nothing -> pure state

-- When runGen is called from the main thread, we want to use
-- postGUIAsync, but when it's called from any other thread, we want to
-- use postGUISync - so we provide an argument to select the function.
-- See http://gtk2hs-users.narkive.com/QvCQw4q3/use-of-postguisync-within-the-main-gtk-thread
runGen :: Application -> (IO () -> IO ()) -> IO ()
runGen app postFn = do
    modifyState app $ \state ->
        let r = state ^. rule
            (g, s) = state ^. currentPattern
        in state & currentPattern .~ runRand (evolve r g) s
    modifyGeneration app (+1)
    postFn (widgetQueueDraw $ app ^. canvas)
