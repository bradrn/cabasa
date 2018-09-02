{-# LANGUAGE LambdaCase #-}

module ControlButtons where

import Control.Concurrent
import Control.Monad (forever)
import Data.IORef

import Data.Text (pack)
import Graphics.UI.Gtk
import Lens.Micro

import Common
import Types

addControlButtonHandlers :: Application -> IO ()
addControlButtonHandlers app = do
    _ <- (app ^. step) `on` buttonActivated $ savePattern app >> runGen app postGUIAsync
    _ <- (app ^. run) `on` buttonActivated $ readIORef (app ^. runThread) >>= \case
        Just t -> do
            killThread t
            writeIORef (app ^. runThread) Nothing
            imageSetFromStock (app ^. runIcon) (pack "gtk-media-play") IconSizeButton
        Nothing -> do
            savePattern app
            t <- forkIO $ forever $ runGen app postGUISync >> threadDelay 100000
            writeIORef (app ^. runThread) $ Just t
            imageSetFromStock (app ^. runIcon) (pack "gtk-media-pause") IconSizeButton
    _ <- (app ^. reset) `on` buttonActivated $ do
        readIORef (app ^. runThread) >>= \case
            Just t -> do
                killThread t
                writeIORef (app ^. runThread) Nothing
                imageSetFromStock (app ^. runIcon) (pack "gtk-media-play") IconSizeButton
            Nothing -> return ()
        popPattern app
        widgetQueueDraw (app ^. canvas)
    return ()
