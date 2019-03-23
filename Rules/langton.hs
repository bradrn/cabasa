{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Life where

import CA.Universe
import CA.Utils
import Hint.Interop

deriving instance Eq Direction
data Color = White | Black deriving (Eq, Show)
data State = State Color | Ant Color Direction deriving (Eq, Show)

color2int :: Color -> Int
color2int White = 0
color2int Black = 1

dir2int :: Direction -> Int
dir2int LeftDir  = 2
dir2int RightDir = 3
dir2int UpDir    = 4
dir2int DownDir  = 5

int2color :: Int -> Color
int2color 1 = Black
int2color 0 = White
int2color _ = error "invalid color!"

int2dir :: Int -> Direction
int2dir 2 = LeftDir
int2dir 3 = RightDir
int2dir 4 = UpDir
int2dir 5 = DownDir
int2dir _ = error "invalid direction!"

getdir :: State -> Maybe Direction
getdir (State _) = Nothing
getdir (Ant c d) = Just $ case c of
    White -> case d of
        LeftDir  -> UpDir
        UpDir    -> RightDir
        RightDir -> DownDir
        DownDir  -> LeftDir
    Black -> case d of
        LeftDir  -> DownDir
        DownDir  -> RightDir
        RightDir -> UpDir
        UpDir    -> LeftDir

myCA :: CAVals
myCA = CAVals $ CAVals' {..}
  where
    _rule = (pure.) . langton
    _states = [State White, State Black] ++
              [Ant c d | d <- [LeftDir, RightDir, UpDir, DownDir], c <- [White, Black]]
    _defaultSize = (100,100)
    _defaultVal  = const (State White)
    _state2color (State White) = (1,1,1)
    _state2color (State Black) = (0,0,0)
    _state2color (Ant White _) = (0,1,1)
    _state2color (Ant Black _) = (1,0,0)
    _encodeInt (State c) = color2int c
    _encodeInt (Ant c d) = (2 * dir2int d) + (color2int c)
    _decodeInt 0 = State White
    _decodeInt 1 = State Black
    _decodeInt n = Ant (int2color $ n `rem` 2) (int2dir $ n `quot` 2)
    _getName = Just . show

langton :: Point -> Universe State -> State
langton p u =
    case (peek p u) of
        Ant White _ -> State Black
        Ant Black _ -> State White
        State c | getdir (peek (move UpDir    p) u) == Just DownDir  -> Ant c DownDir
                | getdir (peek (move DownDir  p) u) == Just UpDir    -> Ant c UpDir
                | getdir (peek (move LeftDir  p) u) == Just RightDir -> Ant c RightDir
                | getdir (peek (move RightDir p) u) == Just LeftDir  -> Ant c LeftDir
                | otherwise -> State c
