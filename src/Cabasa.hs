{-# LANGUAGE CPP                                                #-}
{-# LANGUAGE DataKinds                                          #-}
{-# LANGUAGE FlexibleContexts                                   #-}
{-# LANGUAGE LambdaCase                                         #-}
{-# LANGUAGE OverloadedLabels                                   #-}
{-# LANGUAGE OverloadedStrings                                  #-}
{-# LANGUAGE RankNTypes                                         #-}
{-# LANGUAGE RecordWildCards                                    #-}
{-# LANGUAGE ScopedTypeVariables                                #-}
{-# LANGUAGE TypeApplications                                   #-}
{-# OPTIONS_GHC -Werror=missing-fields -fno-warn-unused-do-bind #-}

module Cabasa (launchCabasa) where

import Control.Arrow ((&&&))
import Control.Concurrent (ThreadId)
import Control.Monad ((>=>), replicateM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int32)
import Data.IORef

import Control.Monad.Random.Strict (getStdGen, newStdGen, randomRs)
import Data.Array (array)
import Data.Finite (Finite)
import Data.Text hiding (count)
import Data.GI.Base.GType (gtypeInt)
import Data.GI.Gtk.BuildFn
import GI.Gtk hiding (init, main)
import qualified GI.Gtk as G
import GI.Gdk (screenGetDefault)
import Lens.Micro

import CA.Core (pureRule, peek, CARuleA)
import CA.Universe (Universe(Universe), Coord(..), Point, Axis(..), Point(Point))
import CA.Utils (moore, count)
import Cabasa.Handlers
import qualified Cabasa.Types as T
import qualified Cabasa.Types.Application as T
import Control.Monad.App (runApp)
import Paths_cabasa

launchCabasa :: T.RuleConfig t -> IO ()
launchCabasa ruleConfig = do
    G.init Nothing
    builder <- builderNew
    builderAddFromFile builder . pack =<< getDataFileName "cabasa.glade"
    prov <- cssProviderNew
    cssProviderLoadFromPath prov . pack =<< getDataFileName "cabasa.css"
    screenGetDefault >>= \case
        Just screen -> styleContextAddProviderForScreen screen prov 800
        Nothing -> return ()

    guiObjects <- buildWithBuilder buildUI builder

    s <- getStdGen
    _currentPattern        <- newIORef (defaultPattern (T._defaultSize ruleConfig) (T._defaultVal ruleConfig), s)

    _saved                 <- newIORef Nothing
    _clipboardContents     <- newIORef Nothing
    _currentPatternPath    <- newIORef @(Maybe String) Nothing
    _currentRulePath       <- newIORef @(Maybe String) Nothing
    _currentStylesheetPath <- newIORef @(Maybe String) Nothing
    _pos <- newIORef $
        T.Pos { _leftXCoord = 0, _topYCoord = 0, _cellWidth = 16, _cellHeight = 16 }
    _runThread             <- newIORef @(Maybe ThreadId) Nothing
    _lastPoint             <- newIORef @(Maybe CA.Universe.Point) Nothing
    _selection             <- newIORef Nothing
    _pasteSelectionOverlay <- newIORef Nothing
    _generation            <- newIORef @Int 0
    _delay                 <- newIORef @Int 100000
    _currentMode           <- newIORef T.DrawMode

    let ioRefs = T.IORefs{..}

    colors <- randomColors

    let app =
          T.Application { T._colors        = colors
                        , T._appGuiObjects = guiObjects
                        , T._appIORefs     = ioRefs
                        , T._appRuleConfig = ruleConfig
                        }

    runApp addHandlers app

    on (guiObjects ^. T.window) #destroy $ mainQuit
    widgetShowAll (guiObjects ^. T.window)
    G.main

-- | Get the actual pattern from the info stored in a 'CAVals''
defaultPattern :: (Coord 'X, Coord 'Y) -> (Point -> t) -> Universe t
defaultPattern s v = Universe $ array (bounds s) (mkPoints s v)
  where
      mkPoints :: (Coord 'X, Coord 'Y) -> (Point -> t) -> [(Point, t)]
      mkPoints (w,h) getVal =
          let ps = Point <$> [0..w-1] <*> [0..h-1]
          in (id &&& getVal) <$> ps

      bounds :: (Coord 'X, Coord 'Y) -> (Point, Point)
      bounds (w,h) = (Point 0 0, Point (w-1) (h-1))

randomColors :: IO [(Double, Double, Double)]
randomColors = fmap (([(1, 1, 1), (0, 0, 0)]++) . tuplize) $ replicateM 3 $ randomRs @Double (0, 1) <$> newStdGen
  where
    tuplize [(a:as), (b:bs), (c:cs)] = (a, b, c):tuplize [as, bs, cs]
    tuplize _ = error "ERROR in tuplize"

buildUI :: BuildFn T.GuiObjects
buildUI = do
    _window <- getObject Window      "window"

    ------- Menu widgets --------------

    _savePattern   <- getObject MenuItem "savePattern"
    _savePatternAs <- getObject MenuItem "savePatternAs"
    _openPattern   <- getObject MenuItem "openPattern"
    _quit          <- getObject MenuItem "quit"
    _cutCanvas     <- getObject MenuItem "cutCanvas"
    _copyCanvas    <- getObject MenuItem "copyCanvas"
    _pasteToCanvas <- getObject MenuItem "pasteToCanvas"
    _changeGridSize <- getObject MenuItem "changeGridSize"
    _goFaster      <- getObject MenuItem "goFaster"
    _goSlower      <- getObject MenuItem "goSlower"
    _clearPattern  <- getObject MenuItem "clearPattern"
    _clearSelection <- getObject MenuItem "clearSelection"
    _drawMode      <- getObject MenuItem "drawMode"
    _moveMode      <- getObject MenuItem "moveMode"
    _selectMode    <- getObject MenuItem "selectMode"
    _about         <- getObject MenuItem "about"
    _uman          <- getObject MenuItem "uman"

    ------- Main window widgets -------

    _run           <- getObject Button      "run"
    _runIcon       <- getObject Image       "runIcon"
    _step          <- getObject Button      "step"
    _reset         <- getObject Button      "reset"
    _canvas        <- getObject DrawingArea "canvas"
    _generationLbl <- getObject Label       "generation"
    _coordsLbl     <- getObject Label       "coords"
    _delayLbl      <- getObject Label       "delay"
    _drawopts      <- getObject Box         "drawopts"
    _curstate      <- getObject ComboBox    "curstate"

    _curstatem <- listStoreNew [gtypeInt]
    liftIO $ do
        forM_ [0,1] $ toGValue @Int32 >=> \x ->
            listStoreInsertWithValuesv _curstatem (-1) [0] [x]
        comboBoxSetModel _curstate (Just _curstatem)
        cellLayoutClear _curstate
        curstateRenderer <- cellRendererTextNew
        cellLayoutPackStart _curstate curstateRenderer True
        cellLayoutClearAttributes _curstate curstateRenderer
        cellLayoutAddAttribute _curstate curstateRenderer "text" 0

    ------- New grid size dialog -----------
    _newGridSizeDialog    <- getObject Dialog     "newGridSizeDialog"
    _newNumColsAdjustment <- getObject Adjustment "newNumColsAdjustment"
    _newNumRowsAdjustment <- getObject Adjustment "newNumRowsAdjustment"

    return T.GuiObjects{..}
