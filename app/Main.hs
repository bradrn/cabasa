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
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main (main) where

import Control.Concurrent (ThreadId)
import Control.Monad ((=<<))
import Data.IORef

import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.CssProvider
import Graphics.UI.Gtk.General.StyleContext

import CA hiding (pos)
import CA.Utils (conwayLife)
import Canvas
import ControlButtons
import Hint.Interop
import Menu
import Paths_cabasa
import SetRuleWindow
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

    _savePatternAs <- builderGetObject builder castToMenuItem "savePatternAs"
    _openPattern   <- builderGetObject builder castToMenuItem "openPattern"
    _quit          <- builderGetObject builder castToMenuItem "quit"
    _setRule       <- builderGetObject builder castToMenuItem "setRule"
    _clearPattern  <- builderGetObject builder castToMenuItem "clearPattern"
    _drawMode      <- builderGetObject builder castToMenuItem "drawMode"
    _moveMode      <- builderGetObject builder castToMenuItem "moveMode"
    _about         <- builderGetObject builder castToMenuItem "about"
    _uman          <- builderGetObject builder castToMenuItem "uman"

    ------- Main window widgets -------

    _run           <- builderGetObject builder castToButton      "run"
    _runIcon       <- builderGetObject builder castToImage       "runIcon"
    _step          <- builderGetObject builder castToButton      "step"
    _reset         <- builderGetObject builder castToButton      "reset"
    _canvas        <- builderGetObject builder castToDrawingArea "canvas"
    _generationLbl <- builderGetObject builder castToLabel       "generation"
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
    _saveRuleAs    <- builderGetObject builder castToMenuItem      "saveRuleAs"
    _openRule      <- builderGetObject builder castToMenuItem      "openRule"

    let guiObjects = T.GuiObjects{..}

    _existState <- do
        s <- getStdGen
        let _rule = pure . conwayLife
            _states = [False, True]
            _defaultPattern = fromList $ replicate 100 $ replicate 100 False
            _state2color st = if st then (0,0,0) else (1,1,1)
            _encodeInt = fromEnum
            _decodeInt 1 = True
            _decodeInt _ = False
            _currentPattern = (fromList $ replicate 100 $ replicate 100 False, s)
            _saved = Nothing
        newIORef $ T.ExistState (T.ExistState'{_ca=CAVals'{..}, ..})
    _currentRuleName <- newIORef @(Maybe String) Nothing
    _pos             <- newIORef $
        T.Pos { _leftXCoord = 0, _topYCoord = 0, _cellWidth = 16, _cellHeight = 16 }
    _runThread       <- newIORef @(Maybe ThreadId) Nothing
    _lastPoint       <- newIORef @(Maybe CA.Point) Nothing
    _generation      <- newIORef @Int 0
    _currentMode     <- newIORef T.DrawMode

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

    _window `on` objectDestroy $ mainQuit
    widgetShowAll _window
    mainGUI

randomColors :: IO [(Double, Double, Double)]
randomColors = fmap (([(1, 1, 1), (0, 0, 0)]++) . tuplize) $ replicateM 3 $ randomRs @Double (0, 1) <$> newStdGen
  where
    tuplize [(a:as), (b:bs), (c:cs)] = (a, b, c):tuplize [as, bs, cs]
    tuplize _ = error "ERROR in tuplize"
