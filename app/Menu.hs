{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Menu
    ( savePattern
    , savePatternAs
    , openPattern
    , copyCanvas
    , cutCanvas
    , changeGridSize
    ) where

import Control.Monad (when, void)
import Data.Bifunctor (first)
import Data.Ix (range)

import qualified CA.Format.MCell as MC
import Data.Text (pack, unpack)
import Lens.Micro hiding (set)
import System.FilePath (takeExtension, (-<.>))

import CA.Core (peek, evolve)
import CA.Types (Point(Point), Coord(Coord), Axis(X, Y), Universe)
import CA.Universe (render, fromList, size, clipInside, Bounds(..))
import Control.Monad.App.Class
import qualified Types as T

copyCanvas :: MonadApp m => m ()
copyCanvas = getSelection >>= \case
    Nothing -> pure ()    -- Can't copy when there's no selection!
    Just ps -> doCopy ps

cutCanvas :: MonadApp m => m ()
cutCanvas = getSelection >>= \case
    Nothing -> pure ()    -- Can't cut when there's no selection!
    Just ps@(Point x1 y1, Point x2 y2) ->
        let lo = Point (min x1 x2) (min y1 y2)
            hi = Point (max x1 x2) (max y1 y2)
            inSelPs = range (lo,hi)
        in do
            doCopy ps
            getOps >>= \case
                Ops{..} -> modifyPattern $ curry $ first $ evolve $
                    \p val ->
                        if p `elem` inSelPs
                        then defaultVal p
                        else peek p val

doCopy :: MonadApp m
       => (Point, Point)  -- ^ Selection
       -> m ()
doCopy (Point x1 y1, Point x2 y2) = getOps >>= \case
    Ops{..} ->
        let (_, vals) = clipInside getPattern Bounds
                { boundsLeft   = min x1 x2
                , boundsRight  = max x1 x2
                , boundsTop    = min y1 y2
                , boundsBottom = max y1 y2
                }
        in setClipboard (Just $ fromList vals)

changeGridSize :: MonadApp m => m ()
changeGridSize = getOps >>= \case
    Ops{..} ->
        runGridSizeDialog (size getPattern) $ \cols rows ->
            modifyPattern $ curry $ first $ changeGridTo (cols, rows) defaultVal
 where
   changeGridTo :: forall a. (Coord 'X, Coord 'Y) -> (Point -> a) -> Universe a -> Universe a
   changeGridTo (newCols, newRows) def u =
       let u' = render u
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

savePattern :: MonadApp m => m ()
savePattern = getCurrentPatternPath >>= \case
    Nothing   -> savePatternAs
    Just path -> writeCurrentPattern path

savePatternAs :: MonadApp m => m ()
savePatternAs = void $ withPatternFileDialog SaveFile $ const writeCurrentPattern

writeCurrentPattern :: MonadApp m => FilePath -> m ()
writeCurrentPattern fName = getOps >>= \case
    Ops{..} -> do
        ruleName <- getCurrentRuleName
        let mc = MC.MCell { MC.game = Just MC.SpecialRules
                          , MC.rule = ruleName
                          , MC.speed = Nothing
                          , MC.ccolors = Just (length states)
                          , MC.coloring = Nothing
                          , MC.wrap = Just True
                          , MC.palette = Nothing
                          , MC.description = Nothing
                          , MC.universe = encodeInt <$> getPattern
                          , MC.diversities = []
                          }
        writePattern (fName -<.> "mcl") $ MC.encodeMCell mc

openPattern :: MonadApp m => m ()
openPattern = void $
    withPatternFileDialog OpenFile $ \pat fName -> do
        case MC.decodeMCell (unpack pat) of
            Left err -> showErrorDialog $ "Could not decode file! The error was:\n" <> pack err
            Right MC.MCell{MC.rule=rule, MC.universe=universe} -> do
                curRuleName <- getCurrentRuleName
                whenMaybeM rule $ \rule' ->
                    when (maybe True (rule'==) curRuleName) $ do
                        showQueryDialog 
                            "This pattern is set to use a different rule to the rule currently loaded\nDo you want to change the rule to that specified in the pattern?"
                            (return ()) $ do
                                rulePath <- locateRuleByName rule' $
                                    showQueryDialog
                                        ("Could not find the specified rule '" <> pack rule' <> "'.\nDo you want to find this rule manually?")
                                        (return Nothing) $
                                        withRuleFileDialog OpenFile Nothing $ const (const pure)
                                case rulePath of
                                    Nothing -> pure ()
                                    Just (file, contents) -> do
                                        let ruleType = case (takeExtension file) of
                                                ".hs"  -> T.Hint
                                                ".alp" -> T.ALPACA
                                                _ -> T.ALPACA -- guess
                                        setCurrentRule (Just file) (unpack contents) ruleType
                getOps >>= \Ops{..} -> modifyPattern $ curry $ first $ const $ decodeInt <$> universe
                setCurrentPatternPath fName
  where
    whenMaybeM :: Applicative m => Maybe a -> (a -> m ()) -> m ()
    whenMaybeM (Just a) am = am a
    whenMaybeM Nothing  _  = pure ()
