{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module ControlButtons where

import CA.Universe (evolveA)
import Control.Monad.Random.Strict (runRand)
import qualified Data.Finite as F

import Control.Monad.App.Class
import Types.Application (RuleConfig(..))

runButtonHandler :: (EvolutionSettings m, HasRuleConfig n a m, Pattern (F.Finite n) m, PlayThread m, SaveRestorePattern m) => m ()
runButtonHandler = togglePlayThread saveRestorePattern runGen

resetButtonHandler :: (EvolutionSettings m, PlayThread m, SaveRestorePattern m) => m ()
resetButtonHandler = do
    forceKillThread
    modifyGen $ const 0
    restorePattern
    
runGen :: (EvolutionSettings m, HasRuleConfig n a m, Pattern (F.Finite n) m) => m ()
runGen = do
    RuleConfig{_rule} <- askRuleConfig
    modifyPattern $ runRand . evolveA _rule
    modifyGen (+1)
