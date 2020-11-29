{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cabasa.Menu
    ( savePattern
    , savePatternAs
    , openPattern
    , copyCanvas
    , cutCanvas
    , changeGridSize
    ) where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Ix (range)

import qualified CA.Format.MCell as MC
import Data.Text (pack, unpack)
import Lens.Micro hiding (set)
import System.FilePath ((-<.>))

import CA.Core (peek, evolve)
import CA.Types (Point(Point), Coord(Coord), Axis(X, Y), Universe)
import CA.Universe (toList, fromList, size, clipInside, Bounds(..))
import Control.Monad.App.Class

import Cabasa.Types (RuleConfig(..))

copyCanvas :: (Canvas m, Clipboard a m, Pattern a m) => m ()
copyCanvas = getSelection >>= \case
    Nothing -> pure ()    -- Can't copy when there's no selection!
    Just ps -> doCopy ps

cutCanvas :: (Canvas m, Clipboard a m, HasRuleConfig a m, Pattern a m) => m ()
cutCanvas = getSelection >>= \case
    Nothing -> pure ()    -- Can't cut when there's no selection!
    Just ps@(Point x1 y1, Point x2 y2) ->
        let lo = Point (min x1 x2) (min y1 y2)
            hi = Point (max x1 x2) (max y1 y2)
            inSelPs = range (lo,hi)
        in do
            doCopy ps
            RuleConfig{_defaultVal} <- askRuleConfig
            modifyPattern $ curry $ first $ evolve $
               \p val ->
                   if p `elem` inSelPs
                   then _defaultVal p
                   else peek p val

doCopy :: (Clipboard a m, Pattern a m)
       => (Point, Point)  -- ^ Selection
       -> m ()
doCopy (Point x1 y1, Point x2 y2) = do
    _pattern <- getPattern
    let (_, vals) = clipInside _pattern Bounds
            { boundsLeft   = min x1 x2
            , boundsRight  = max x1 x2
            , boundsTop    = min y1 y2
            , boundsBottom = max y1 y2
            }
    setClipboard (Just $ fromList vals)

changeGridSize :: (HasRuleConfig a m, Pattern a m, Windows m) => m ()
changeGridSize = do
    RuleConfig{_defaultVal} <- askRuleConfig
    _pattern <- getPattern
    runGridSizeDialog (size _pattern) $ \cols rows ->
        modifyPattern $ curry $ first $ changeGridTo (cols, rows) _defaultVal
 where
   changeGridTo :: forall a. (Coord 'X, Coord 'Y) -> (Point -> a) -> Universe a -> Universe a
   changeGridTo (newCols, newRows) def u =
       let u' = toList u
           (oldCols, oldRows) = size u
           dCols = newCols - oldCols
           dRows = newRows - oldRows

           withExtraRows = addRows dRows u'
           withExtraCols = addCols dCols withExtraRows
       in fromList withExtraCols
     where
         imap :: (Int -> x -> y) -> [x] -> [y]
         imap f = snd . foldr (\val (i,acc) -> (i+1, (f i val):acc)) (0, [])

         addCols :: Coord 'X -> [[a]] -> [[a]]
         addCols 0 = id
         addCols (Coord n)
             | n < 0 = fmap $ \row -> take (n + length row) row
             | otherwise = imap $ \i row ->
                   let firstNewCol = length row
                       newColNs = Coord @'X <$> [firstNewCol .. (firstNewCol + n - 1)]
                       newColVals = newColNs <&> \col -> def (Point col (Coord i))
                   in row ++ newColVals

         addRows :: Coord 'Y -> [[a]] -> [[a]]
         addRows 0 u' = u'
         addRows (Coord n) u'
             | n < 0 = take (n + length u') u'
             | otherwise =
                   let width = Coord @'X $ length (head u') - 1
                       firstNewRow = length u'
                       newRowNs = Coord @'Y <$> [firstNewRow .. (firstNewRow + n - 1)]
                       newRowVals = newRowNs <&> \row ->
                           [0..width] <&> \col -> def (Point col row)
                   in u' ++ newRowVals

savePattern :: (HasRuleConfig a m, Paths m, Pattern a m, Windows m) => m ()
savePattern = getCurrentPatternPath >>= \case
    Nothing   -> savePatternAs
    Just path -> writeCurrentPattern path

savePatternAs :: (HasRuleConfig a m, Paths m, Pattern a m, Windows m) => m ()
savePatternAs = void $ withPatternFileDialog SaveFile $ const writeCurrentPattern

writeCurrentPattern :: (Paths m, HasRuleConfig a m, Pattern a m) => FilePath -> m ()
writeCurrentPattern fName = do
    ruleName <- getCurrentRuleName
    _pattern <- getPattern
    _encodeInt <- encodeInt
    _states <- states
    let mc = MC.MCell { MC.game = Just MC.SpecialRules
                        , MC.rule = ruleName
                        , MC.speed = Nothing
                        , MC.ccolors = Just (length _states)
                        , MC.coloring = Nothing
                        , MC.wrap = Just True
                        , MC.palette = Nothing
                        , MC.description = Nothing
                        , MC.universe = _encodeInt <$> _pattern
                        , MC.diversities = []
                        }
    writePattern (fName -<.> "mcl") $ MC.encodeMCell mc

openPattern ::
    ( Canvas m
    , EvolutionSettings m
    , Files m
    , HasRuleConfig a m
    , Paths m
    , Pattern a m
    , SaveRestorePattern m
    , Windows m
    ) => m ()
openPattern = void $
    withPatternFileDialog OpenFile $ \pat fName -> do
        case MC.decodeMCell (unpack pat) of
            Left err -> showErrorDialog $ "Could not decode file! The error was:\n" <> pack err
            Right MC.MCell{MC.rule=rule, MC.universe=universe} -> do
                curRuleName <- getCurrentRuleName
                _decodeInt <- decodeInt
                whenMaybeM rule $ \rule' ->
                    case curRuleName of
                        Just r | rule' /= r -> do
                            modifyPattern $ curry $ first $ const $ _decodeInt <$> universe
                            setCurrentPatternPath fName
                        _ -> showQueryDialog
                            "This pattern is set to use a different rule to the rule currently loaded\nAre you sure you want to open it?"
                            (return ()) $ do
                                modifyPattern $ curry $ first $ const $ _decodeInt <$> universe
                                setCurrentPatternPath fName
  where
    whenMaybeM :: Applicative m => Maybe a -> (a -> m ()) -> m ()
    whenMaybeM (Just a) am = am a
    whenMaybeM Nothing  _  = pure ()
