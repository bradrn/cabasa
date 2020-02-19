{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}

module ControlButtons where

import CA.Universe (evolveA)
import Control.Monad.Random.Strict (runRand)

import Control.Monad.App.Class

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
