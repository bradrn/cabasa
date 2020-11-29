{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Types where

import Control.Arrow ((&&&))
import Data.Functor.Const

import CA.Universe
import Control.Monad.Random.Strict (StdGen, Rand)
import Data.Array (array)
import Data.Finite (Finite)
import Lens.Micro
import Lens.Micro.TH (makeLenses, makeClassy)

data InteractionMode
    = DrawMode
    -- ^ When the user click/drags, draw on the screen
    | MoveMode
    -- ^ When the user click/drags, move the grid
    | SelectMode
    -- ^ When the user click/drags, draw a selection box
    | PastePendingMode InteractionMode
    -- ^ When the user clicks, paste the clipboard contents, then
    -- restore the previous mode (stored as the parameter)
    deriving (Eq)

data Pos = Pos { _leftXCoord :: Coord 'X
               , _topYCoord  :: Coord 'Y
               , _cellWidth  :: Double
               , _cellHeight :: Double
               }

makeLenses ''Pos

data RuleConfig n = RuleConfig
    { _rule :: Point -> Universe (Finite n) -> Rand StdGen (Finite n)  -- ^ The rule itself
    , _defaultSize :: (Coord 'X, Coord 'Y)           -- ^ Default (width, height) of grid
    , _defaultVal  :: Point -> Finite n                     -- ^ The default value at each point
    , _state2color :: Finite n -> (Double, Double, Double)  -- ^ A function to convert states into (red, green, blue) colours which are displayed on the grid
    }

makeClassy ''RuleConfig

-- | Get the actual pattern from the info stored in a 'CAVals''
_defaultPattern :: (Coord 'X, Coord 'Y) -> (Point -> t) -> Universe t
_defaultPattern s v = Universe $ array (bounds s) (mkPoints s v)
  where
      mkPoints :: (Coord 'X, Coord 'Y) -> (Point -> t) -> [(Point, t)]
      mkPoints (w,h) getVal =
          let ps = Point <$> [0..w-1] <*> [0..h-1]
          in (id &&& getVal) <$> ps

      bounds :: (Coord 'X, Coord 'Y) -> (Point, Point)
      bounds (w,h) = (Point 0 0, Point (w-1) (h-1))

defaultPattern :: SimpleGetter (RuleConfig n) (Universe (Finite n))
defaultPattern = \out vals ->
    let s = vals ^. defaultSize
        v = vals ^. defaultVal
    in retag $ out $ _defaultPattern s v
  where
    retag :: Const x a -> Const x b
    retag (Const x) = Const x
