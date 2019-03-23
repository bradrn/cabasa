{-# LANGUAGE RecordWildCards #-}

module Life where

import CA.Universe
import CA.Utils hiding (conwayLife)
import Hint.Interop

data State = Alive | Dead deriving (Eq)

myCA :: CAVals
myCA = CAVals $ CAVals' {..}
  where
    _rule = (pure.) . conwayLife
    _states = [Dead, Alive]
    _defaultSize = (100,100)
    _defaultVal  = const Dead
    _state2color Dead  = (1,1,1)
    _state2color Alive = (0,0,0)
    _encodeInt Dead  = 0
    _encodeInt Alive = 1
    _decodeInt 1 = Alive
    _decodeInt _ = Dead
    _getName = const Nothing

conwayLife :: Point -> Universe State -> State
conwayLife p u =
    let aliveCount = count (==Alive) $ experiment (moore False) p u in
        case peek p u of
            Dead ->  if aliveCount == 3          then Alive else Dead
            Alive -> if aliveCount `elem` [2, 3] then Alive else Dead
