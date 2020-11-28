{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}

module Types.Application where

import Control.Arrow ((&&&))
import Control.Concurrent (ThreadId)
import Data.Functor.Const
import Data.IORef

import CA.Universe
import Control.Monad.Random.Strict (StdGen, Rand)
import Data.Array (array)
import Data.Finite (Finite)
import GI.Gtk hiding (Settings, Application)
import Language.Haskell.TH.Syntax (mkName)
import Lens.Micro
import Lens.Micro.TH (makeClassy, classyRules, lensClass, makeLenses, makeLensesWith)
import System.FilePath (takeBaseName)

import Types

data Application n = Application
    { -- These three fields need to be declared with an 'app' prefix so that e.g.
      -- the generated 'appGuiObjects' lenses don't conflict with the
      -- 'guiObjects' method in the generated 'HasGuiObjects' class
      _appGuiObjects :: GuiObjects
    , _appIORefs     :: IORefs n
    , _appRuleConfig :: RuleConfig n

      -- A list of random colors. There isn't any other good place to put this,
      -- so we'll stick it in here.
    , _colors     :: [(Double, Double, Double)]
    }

data GuiObjects = GuiObjects
    { _window                :: Window
    , _savePattern           :: MenuItem
    , _savePatternAs         :: MenuItem
    , _openPattern           :: MenuItem
    , _quit                  :: MenuItem
    , _cutCanvas             :: MenuItem
    , _copyCanvas            :: MenuItem
    , _pasteToCanvas         :: MenuItem
    , _changeGridSize        :: MenuItem
    , _goFaster              :: MenuItem
    , _goSlower              :: MenuItem
    , _clearPattern          :: MenuItem
    , _clearSelection        :: MenuItem
    , _drawMode              :: MenuItem
    , _moveMode              :: MenuItem
    , _selectMode            :: MenuItem
    , _about                 :: MenuItem
    , _uman                  :: MenuItem
    , _run                   :: Button
    , _runIcon               :: Image
    , _step                  :: Button
    , _reset                 :: Button
    , _canvas                :: DrawingArea
    , _generationLbl         :: Label
    , _coordsLbl             :: Label
    , _delayLbl              :: Label
    , _drawopts              :: Box
    , _curstate              :: ComboBox
    , _curstatem             :: ListStore
    , _numColsAdjustment     :: Adjustment
    , _numRowsAdjustment     :: Adjustment
    , _newGridSizeDialog     :: Dialog
    , _newNumColsAdjustment  :: Adjustment
    , _newNumRowsAdjustment  :: Adjustment
    }

data RuleConfig n = RuleConfig
    { _rule :: Point -> Universe (Finite n) -> Rand StdGen (Finite n)  -- ^ The rule itself
    , _defaultSize :: (Coord 'X, Coord 'Y)           -- ^ Default (width, height) of grid
    , _defaultVal  :: Point -> Finite n                     -- ^ The default value at each point
    , _state2color :: Finite n -> (Double, Double, Double)  -- ^ A function to convert states into (red, green, blue) colours which are displayed on the grid
    }

data IORefs n = IORefs
  {
    _currentPattern        :: IORef (Universe (Finite n), StdGen)
  , _currentRulePath       :: IORef (Maybe FilePath)   -- The name of the current rule
  , _currentPatternPath    :: IORef (Maybe FilePath)   -- The path of the current pattern
  , _generation            :: IORef Int                -- The current generation
  , _currentMode           :: IORef InteractionMode    -- The current mode
  , _delay                 :: IORef Int                -- The number of microseconds between evolutions

    -- The current state the grid is displayed in: the x-coordinate of the
    -- leftmost column, the y-coordinate of the topmost row, and the width and
    -- height of each cell in pixels
  , _pos                   :: IORef Pos

    -- The thread on which the evolution process is running. If it is paused or
    -- stopped this is equal to Nothing.
  , _runThread             :: IORef (Maybe ThreadId)

    -- If the mouse is pressed and drawing on the screen, this is the last point
    -- which the mouse was on. Note that this is relative to the top left corner
    -- of the screen, not the portion of the grid which is showing, so if the
    -- top left corner is showing the cell at (3, 8) but the mouse is also at
    -- this point then lastPoint is (0, 0) and not (3, 8).
  , _lastPoint             :: IORef (Maybe CA.Universe.Point)

    -- The current selection, if selection mode is enabled. Comprised of a tuple
    -- of two corners of the selection - exactly which corners these are is not
    -- fixed and is based on user input, with the tuple being the (first,last)
    -- point selected. Unlike '_lastPoint' this is relative to the grid, not the
    -- screen.
  , _selection             :: IORef (Maybe (CA.Universe.Point, CA.Universe.Point))

    -- When in 'PastePendingMode', we want to display an overlay where the
    -- clipboard contents must be placed. This controls where this overlay is
    -- drawn. See '_selection' for more details.
  , _pasteSelectionOverlay :: IORef (Maybe (CA.Universe.Point, CA.Universe.Point))

  -- The grid which is to be restored when the 'reset' button is pressed.
  , _saved                 :: IORef (Maybe (Universe (Finite n), Pos))

  -- The current contents of the clipboard, if any. GTK does provide
  -- an interface to the OS clipboard, but it's fairly tricky to
  -- use, so we just emulate our own clipboard.
  , _clipboardContents     :: IORef (Maybe (Universe (Finite n)))
  }

-- Basically this is just makeClassy, but we're changing the name of the
-- generated lens because makeClassy does it with the wrong capitalisation
-- (it does e.g. CAVals' -> cAVals' when we want CAVals' -> caVals')
flip makeLensesWith ''IORefs  $ classyRules & lensClass .~ const (Just (mkName "HasIORefs" , mkName "ioRefs" ))

getCurrentRuleName :: Application n -> IO (Maybe String)
getCurrentRuleName app = (fmap . fmap) takeBaseName $ readIORef (app ^. currentPatternPath)

makeClassy ''GuiObjects

makeClassy ''RuleConfig

makeLenses ''Application
instance HasIORefs (Application n) n where ioRefs = appIORefs
instance HasGuiObjects (Application n) where guiObjects = appGuiObjects
instance HasRuleConfig (Application n) n where ruleConfig = appRuleConfig

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
