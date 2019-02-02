{-# LANGUAGE CPP                         #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE MultiWayIf                  #-}
{-# LANGUAGE NamedFieldPuns              #-}
{-# LANGUAGE RankNTypes                  #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TupleSections               #-}
{-# LANGUAGE TypeApplications            #-}
{-# LANGUAGE ViewPatterns                #-}
{-# OPTIONS_GHC -Werror=missing-fields -fno-warn-unused-do-bind #-}

module Main (main) where

import Control.Concurrent (ThreadId)
import Control.Monad ((=<<), replicateM)
import Data.IORef

import Control.Monad.Random.Strict (getStdGen, newStdGen, randomRs)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.CssProvider
import Graphics.UI.Gtk.General.StyleContext

import CA.Core (pureRule)
import CA.Universe (fromList, Point)
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
    initGUI
    builder <- builderNew
    builderAddFromFile builder =<< getDataFileName "cabasa.glade"
    prov <- cssProviderNew
    cssProviderLoadFromPath prov =<< getDataFileName "cabasa.css"
    screenGetDefault >>= \case
        Just screen -> styleContextAddProviderForScreen screen prov 800
        Nothing -> return ()

    _window <- builderGetObject builder castToWindow      "window"

    ------- Menu widgets --------------

    _savePattern   <- builderGetObject builder castToMenuItem "savePattern"
    _savePatternAs <- builderGetObject builder castToMenuItem "savePatternAs"
    _openPattern   <- builderGetObject builder castToMenuItem "openPattern"
    _runSettings   <- builderGetObject builder castToMenuItem "runSettings"
    _quit          <- builderGetObject builder castToMenuItem "quit"
    _copyCanvas    <- builderGetObject builder castToMenuItem "copyCanvas"
    _setRule       <- builderGetObject builder castToMenuItem "setRule"
    _goFaster      <- builderGetObject builder castToMenuItem "goFaster"
    _goSlower      <- builderGetObject builder castToMenuItem "goSlower"
    _clearPattern  <- builderGetObject builder castToMenuItem "clearPattern"
    _drawMode      <- builderGetObject builder castToMenuItem "drawMode"
    _moveMode      <- builderGetObject builder castToMenuItem "moveMode"
    _selectMode    <- builderGetObject builder castToMenuItem "selectMode"
    _editSheet     <- builderGetObject builder castToMenuItem "editSheet"
    _about         <- builderGetObject builder castToMenuItem "about"
    _uman          <- builderGetObject builder castToMenuItem "uman"

    ------- Main window widgets -------

    _run           <- builderGetObject builder castToButton      "run"
    _runIcon       <- builderGetObject builder castToImage       "runIcon"
    _step          <- builderGetObject builder castToButton      "step"
    _reset         <- builderGetObject builder castToButton      "reset"
    _canvas        <- builderGetObject builder castToDrawingArea "canvas"
    _generationLbl <- builderGetObject builder castToLabel       "generation"
    _coordsLbl     <- builderGetObject builder castToLabel       "coords"
    _delayLbl      <- builderGetObject builder castToLabel       "delay"
    _drawopts      <- builderGetObject builder castToBox         "drawopts"
    _curstate      <- builderGetObject builder castToComboBox    "curstate"

    _curstatem <- listStoreNew [0, 1]
    comboBoxSetModel _curstate (Just _curstatem)
    cellLayoutClear _curstate
    curstateRenderer <- cellRendererTextNew
    cellLayoutPackStart _curstate curstateRenderer True
    cellLayoutSetAttributes _curstate curstateRenderer _curstatem (pure . (cellText :=) . show)

    ------- Set new rule dialog -------

    _setRuleWindow <- builderGetObject builder castToWindow        "setRuleWindow"
    _setRuleBtn    <- builderGetObject builder castToButton        "setRuleBtn"
    _newRuleBuf    <- builderGetObject builder castToTextView      "newRuleView" >>= textViewGetBuffer
    _alpacaLang    <- builderGetObject builder castToRadioMenuItem "alpacaLang"
    _haskellLang   <- builderGetObject builder castToRadioMenuItem "haskellLang"
    _saveRule      <- builderGetObject builder castToMenuItem      "saveRule"
    _saveRuleAs    <- builderGetObject builder castToMenuItem      "saveRuleAs"
    _openRule      <- builderGetObject builder castToMenuItem      "openRule"

    ------- ALPACA Stylesheets dialog -------

    _editSheetWindow       <- builderGetObject builder castToWindow   "editSheetWindow"
    _openSheet             <- builderGetObject builder castToMenuItem "openSheet"

    _saveSheet             <- builderGetObject builder castToMenuItem "saveSheet"
    _saveSheetAs           <- builderGetObject builder castToMenuItem "saveSheetAs"
    _sheetBuf              <- builderGetObject builder castToTextView "sheetView" >>= textViewGetBuffer
    _editSheetWindowSetBtn <- builderGetObject builder castToButton   "editSheetWindowSetBtn"

    ------- Settings dialog -----------

    _settingsWindow        <- builderGetObject builder castToDialog            "settingsWindow"
    _settingsCancelBtn     <- builderGetObject builder castToButton            "settingsCancelBtn"
    _settingsOkBtn         <- builderGetObject builder castToButton            "settingsOkBtn"
    _predefRulesDirChooser <- builderGetObject builder castToFileChooserButton "predefRulesDirChooser"
    _userRulesDirChooser   <- builderGetObject builder castToFileChooserButton "userRulesDirChooser"
    _numColsAdjustment     <- builderGetObject builder castToAdjustment        "numColsAdjustment"
    _numRowsAdjustment     <- builderGetObject builder castToAdjustment        "numRowsAdjustment"

    let guiObjects = T.GuiObjects{..}

    _settings   <- newIORef =<< readSettings _window
    _existState <- do
        s <- getStdGen
        (numcols, numrows) <- getSettingFrom' T.gridSize _settings
        let _rule = pureRule conwayLife
            _states = [False, True]
            _defaultPattern = fromList $ replicate numrows $ replicate numcols False
            _state2color st = if st then (0,0,0) else (1,1,1)
            _encodeInt = fromEnum
            _decodeInt 1 = True
            _decodeInt _ = False
            _getName = const Nothing
            _currentPattern = (_defaultPattern, s)
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

    _window `on` objectDestroy $ mainQuit
    widgetShowAll _window
    mainGUI

randomColors :: IO [(Double, Double, Double)]
randomColors = fmap (([(1, 1, 1), (0, 0, 0)]++) . tuplize) $ replicateM 3 $ randomRs @Double (0, 1) <$> newStdGen
  where
    tuplize [(a:as), (b:bs), (c:cs)] = (a, b, c):tuplize [as, bs, cs]
    tuplize _ = error "ERROR in tuplize"
