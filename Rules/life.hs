{-# LANGUAGE RecordWildCards #-}

module Life where

import CA
import CA.Utils hiding (conwayLife)
import Hint.Interop

data State = Alive | Dead deriving (Eq)

myCA :: CAVals
myCA = CAVals $ CAVals' {..}
  where
    _rule = pure . conwayLife
    _states = [Dead, Alive]
    _defaultPattern = fromList $ replicate 100 $ replicate 100 Dead
    _state2color Dead  = (1,1,1)
    _state2color Alive = (0,0,0)
    _encodeInt Dead  = 0
    _encodeInt Alive = 1
    _decodeInt 1 = Alive
    _decodeInt _ = Dead

conwayLife :: Universe State -> State
conwayLife u =
    let aliveCount = count (==Alive) $ experiment (moore False) u in
        case extract u of
            Dead ->  if aliveCount == 3          then Alive else Dead
            Alive -> if aliveCount `elem` [2, 3] then Alive else Dead