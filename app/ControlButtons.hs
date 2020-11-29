{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module ControlButtons where

import CA.Universe (evolveA)
import Control.Monad.Random.Strict (runRand)

import Control.Monad.App.Class
import Types (RuleConfig(..))

runButtonHandler :: (EvolutionSettings m, HasRuleConfig a m, Pattern a m, PlayThread m, SaveRestorePattern m) => m ()
runButtonHandler = togglePlayThread saveRestorePattern runGen

resetButtonHandler :: (EvolutionSettings m, PlayThread m, SaveRestorePattern m) => m ()
resetButtonHandler = do
    forceKillThread
    modifyGen $ const 0
    restorePattern
    
runGen :: (EvolutionSettings m, HasRuleConfig a m, Pattern a m) => m ()
runGen = do
    RuleConfig{_rule} <- askRuleConfig
    modifyPattern $ runRand . evolveA _rule
    modifyGen (+1)
