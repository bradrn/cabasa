{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}

module Types where

import GHC.Generics

import CA.Universe
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Lens.Micro.TH (makeLenses)

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

data Settings = Settings
  {
    -- Predefined rules directory
    _predefinedRulesDir :: Maybe FilePath

    -- User-defined rules directory
  , _userRulesDir       :: Maybe FilePath

    -- Default grid size (rows, columns)
  , _gridSize           :: Maybe (Int, Int)
  } deriving (Generic, Show)

makeLenses ''Settings
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Settings
