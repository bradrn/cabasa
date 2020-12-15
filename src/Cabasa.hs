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

module Cabasa (launchCabasa, PersistMethod(..)) where

import Control.Arrow ((&&&))
import Control.Concurrent (ThreadId)
import Control.Monad ((>=>), replicateM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int32)
import Data.IORef
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Control.DeepSeq (NFData)
import Control.Monad.Random.Strict (getStdGen, newStdGen, randomRs)
import Data.Array (array)
import Data.Finite (Finite)
import Data.Text (pack)
import Data.GI.Base.GType (gtypeInt)
import Data.GI.Gtk.BuildFn
import GI.Gtk hiding (init, main)
import qualified GI.Gtk as G
import GI.Gdk (screenGetDefault)
import Lens.Micro
import System.Directory (doesFileExist)

import CA.Core (pureRule, peek, CARuleA)
import CA.Universe (Universe(Universe), Coord(..), Point, Axis(..), Point(Point), toList, fromList)
import CA.Utils (moore, count)
import Cabasa.Handlers
import qualified Cabasa.Types as T
import qualified Cabasa.Types.Application as T
import Control.Monad.App (runApp)
import Paths_cabasa

-- | Specifies whether (and how) to persist the current pattern
-- between runs.
--
-- (Motivation: in traditional CA simulation software, the pattern
-- stays on the screen as the rule is changed. By contrast, a CA
-- implemented with Cabasa must be closed and recompiled when the rule
-- is changed; in order to imitate this behaviour, Cabasa thus takes
-- the approach of persisting the pattern in a file between runs.)
data PersistMethod
    = NoPersist             -- ^ Disable persistence
    | FilePersist FilePath  -- ^ Enable persistence, storing the pattern in the specified 'FilePath'

-- | Launch the Cabasa GUI. The intended use of this method is to be
-- called from @main@ with the appropriate configuration; Cabasa will
-- then handle all GUI setup and user interaction.
launchCabasa
    :: NFData t
    => PersistMethod   -- ^ Whether to persist pattern state between runs
    -> T.RuleConfig t  -- ^ The CA rule to be used with Cabasa
    -> IO ()
launchCabasa persist ruleConfig = do
    G.init Nothing
    builder <- builderNew
    builderAddFromFile builder . pack =<< getDataFileName "cabasa.glade"
    prov <- cssProviderNew
    cssProviderLoadFromPath prov . pack =<< getDataFileName "cabasa.css"
    screenGetDefault >>= \case
        Just screen -> styleContextAddProviderForScreen screen prov 800
        Nothing -> return ()

    guiObjects <- buildWithBuilder (buildUI $ length $ T._states ruleConfig) builder

    s <- getStdGen
    persisted <- readPersist persist (T._decodeInt ruleConfig)
    _currentPattern        <- newIORef
        ( flip fromMaybe persisted $ defaultPattern (T._defaultSize ruleConfig) (T._defaultVal ruleConfig)
        , s
        )

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

    on (guiObjects ^. T.window) #destroy $ writePersist app persist >> mainQuit
    widgetShowAll (guiObjects ^. T.window)
    G.main

readPersist :: PersistMethod -> (Int -> a) -> IO (Maybe (Universe a))
readPersist NoPersist _ = return Nothing
readPersist (FilePersist file) decodeInt =
    doesFileExist file >>= \case
        False -> return Nothing
        True -> readMaybe <$> readFile file >>= \case
            Nothing -> return Nothing
            Just encodedPattern ->
                return $ Just $ decodeInt <$> fromList encodedPattern

writePersist :: T.Application a -> PersistMethod -> IO ()
writePersist _ NoPersist = pure ()
writePersist app (FilePersist file) = do
    (_pattern, _) <- readIORef $ app ^. T.currentPattern
    let encodedPattern = toList $ (app ^. T.encodeInt) <$> _pattern
    writeFile file $ show encodedPattern
    -- I know 'show' isn’t a very good serialisation method, but it’s
    -- good enough for now and I can always replace it with something
    -- better if needed

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

buildUI
    :: Int  -- ^ Number of states
    -> BuildFn T.GuiObjects
buildUI nss = do
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
        forM_ [0..fromIntegral nss-1] $ toGValue @Int32 >=> \x ->
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
