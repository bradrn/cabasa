{-# LANGUAGE CPP                         #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE MultiWayIf                  #-}
{-# LANGUAGE NamedFieldPuns              #-}
{-# LANGUAGE OverloadedLabels            #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RankNTypes                  #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TupleSections               #-}
{-# LANGUAGE TypeApplications            #-}
{-# LANGUAGE ViewPatterns                #-}
{-# OPTIONS_GHC -Werror=missing-fields -fno-warn-unused-do-bind #-}

module Main (main) where

import Control.Concurrent (ThreadId)
import Control.Monad ((>=>), (=<<), replicateM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int32)
import Data.IORef

import Control.Monad.Random.Strict (getStdGen, newStdGen, randomRs)
import Data.Text
import Data.GI.Gtk.BuildFn
import GI.Gtk hiding (init, main)
import qualified GI.Gtk as G
import GI.Gdk (screenGetDefault)
import Lens.Micro

import CA.Core (pureRule)
import CA.Universe (Coord(..), fromList, Point)
import CA.Utils (conwayLife)
import Canvas
import ControlButtons
import Hint.Interop
import Menu
import Paths_cabasa
import SetRuleWindow
import Settings (getSettingFrom', readSettings)
import StylesheetWindow
import qualified Types as T

main :: IO ()
main = do
    G.init Nothing
    builder <- builderNew
    builderAddFromFile builder . pack =<< getDataFileName "cabasa.glade"
    prov <- cssProviderNew
    cssProviderLoadFromPath prov . pack =<< getDataFileName "cabasa.css"
    screenGetDefault >>= \case
        Just screen -> styleContextAddProviderForScreen screen prov 800
        Nothing -> return ()

    guiObjects <- buildWithBuilder buildUI builder

    _settings   <- newIORef =<< readSettings (guiObjects ^. T.window)
    _existState <- do
        s <- getStdGen
        (numcols, numrows) <- getSettingFrom' T.gridSize _settings
        let _rule = pureRule conwayLife
            _states = [False, True]
            _defaultSize = (Coord numcols, Coord numrows)
            _defaultVal  = const False
            _state2color st = if st then (0,0,0) else (1,1,1)
            _encodeInt = fromEnum
            _decodeInt 1 = True
            _decodeInt _ = False
            _getName = const Nothing
            _currentPattern =
                ( _defaultPattern _defaultSize _defaultVal
                , s
                )
            _saved = Nothing
            _clipboardContents = Nothing
        newIORef $ T.ExistState (T.ExistState'{_ca=CAVals'{..}, ..})
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
                        }

    addCanvasHandlers app

    addControlButtonHandlers app

    addMenuHandlers app

    addSetRuleWindowHandlers app

    addStylesheetWindowHandlers app

    on (guiObjects ^. T.window) #destroy $ mainQuit
    widgetShowAll (guiObjects ^. T.window)
    G.main

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
    _runSettings   <- getObject MenuItem "runSettings"
    _quit          <- getObject MenuItem "quit"
    _cutCanvas     <- getObject MenuItem "cutCanvas"
    _copyCanvas    <- getObject MenuItem "copyCanvas"
    _pasteToCanvas <- getObject MenuItem "pasteToCanvas"
    _changeGridSize <- getObject MenuItem "changeGridSize"
    _setRule       <- getObject MenuItem "setRule"
    _goFaster      <- getObject MenuItem "goFaster"
    _goSlower      <- getObject MenuItem "goSlower"
    _clearPattern  <- getObject MenuItem "clearPattern"
    _clearSelection <- getObject MenuItem "clearSelection"
    _drawMode      <- getObject MenuItem "drawMode"
    _moveMode      <- getObject MenuItem "moveMode"
    _selectMode    <- getObject MenuItem "selectMode"
    _editSheet     <- getObject MenuItem "editSheet"
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

    ------- Set new rule dialog -------

    _setRuleWindow <- getObject Window        "setRuleWindow"
    _setRuleBtn    <- getObject Button        "setRuleBtn"
    _newRuleBuf    <- getObject TextView      "newRuleView" >>= textViewGetBuffer
    _alpacaLang    <- getObject RadioMenuItem "alpacaLang"
    _haskellLang   <- getObject RadioMenuItem "haskellLang"
    _saveRule      <- getObject MenuItem      "saveRule"
    _saveRuleAs    <- getObject MenuItem      "saveRuleAs"
    _openRule      <- getObject MenuItem      "openRule"

    ------- ALPACA Stylesheets dialog -------

    _editSheetWindow       <- getObject Window   "editSheetWindow"
    _openSheet             <- getObject MenuItem "openSheet"

    _saveSheet             <- getObject MenuItem "saveSheet"
    _saveSheetAs           <- getObject MenuItem "saveSheetAs"
    _sheetBuf              <- getObject TextView "sheetView" >>= textViewGetBuffer
    _editSheetWindowSetBtn <- getObject Button   "editSheetWindowSetBtn"

    ------- Settings dialog -----------

    _settingsWindow        <- getObject Dialog            "settingsWindow"
    _settingsCancelBtn     <- getObject Button            "settingsCancelBtn"
    _settingsOkBtn         <- getObject Button            "settingsOkBtn"
    _predefRulesDirChooser <- getObject FileChooserButton "predefRulesDirChooser"
    _userRulesDirChooser   <- getObject FileChooserButton "userRulesDirChooser"
    _numColsAdjustment     <- getObject Adjustment        "numColsAdjustment"
    _numRowsAdjustment     <- getObject Adjustment        "numRowsAdjustment"

    ------- New grid size dialog -----------
    _newGridSizeDialog    <- getObject Dialog     "newGridSizeDialog"
    _newNumColsAdjustment <- getObject Adjustment "newNumColsAdjustment"
    _newNumRowsAdjustment <- getObject Adjustment "newNumRowsAdjustment"

    return T.GuiObjects{..}
