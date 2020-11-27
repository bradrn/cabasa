{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}

module ControlButtons where

import CA.Universe (evolveA)
import Control.Monad.Random.Strict (runRand)

import Control.Monad.App.Class

runButtonHandler :: (EvolutionSettings m, GetOps a m, PlayThread m, SaveRestorePattern m) => m ()
runButtonHandler = togglePlayThread saveRestorePattern runGen

resetButtonHandler :: (EvolutionSettings m, PlayThread m, SaveRestorePattern m) => m ()
resetButtonHandler = do
    forceKillThread
    modifyGen $ const 0
    restorePattern
    
runGen :: (EvolutionSettings m, GetOps a m) => m ()
runGen = do
    getOps >>= \case Ops{..} -> modifyPattern $ runRand . evolveA getRule
    modifyGen (+1)
