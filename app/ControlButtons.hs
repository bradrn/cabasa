{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}

module ControlButtons (addControlButtonHandlers) where

import CA.Universe (evolveA)
import Control.Monad.Random.Strict (runRand)
import GI.Gtk
import Lens.Micro

import Control.Monad.App
import Control.Monad.App.Class
import qualified Types as T

addControlButtonHandlers :: T.Application -> IO ()
addControlButtonHandlers app = do
    _ <- on (app ^. T.step)  #clicked $ runApp (saveRestorePattern >> runGen) app
    _ <- on (app ^. T.run)   #clicked $ runApp runButtonHandler app
    _ <- on (app ^. T.reset) #clicked $ runApp resetButtonHandler app
    return ()

runButtonHandler :: MonadApp m => m ()
runButtonHandler = togglePlayThread saveRestorePattern runGen

resetButtonHandler :: MonadApp m => m ()
resetButtonHandler = do
    forceKillThread
    modifyGen $ const 0
    restorePattern
    
runGen :: MonadApp m => m ()
runGen = do
    getOps >>= \case Ops{..} -> modifyPattern $ runRand . evolveA getRule
    modifyGen (+1)
