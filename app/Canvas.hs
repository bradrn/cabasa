{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Canvas where

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Foldable (for_)
import Data.Functor (($>))

import Data.Array (array, assocs, bounds)
import Data.Text (pack)
import Lens.Micro

import CA.Universe
import Control.Monad.App.Class
import qualified Types as T

clearPattern :: MonadApp m => m ()
clearPattern = do
    modifyGen $ const 0
    modifyPos $ const T.Pos{_leftXCoord=0,_topYCoord=0,_cellWidth=16,_cellHeight=16}
    resetRestorePattern
    getOps >>= \Ops{..} -> modifyPattern $ curry $ first $ const defaultPattern

data MouseGridPos = MouseGridPos
    { gridPos :: Point
      -- ^ The (x,y) coordinates of the cursor position, in grid
      -- coordinates, relative to the top-left of the grid
    , viewPos :: Point
      -- ^ Same as 'gridPos', but relative to the top-left of the
      -- Cabasa grid view
    } deriving (Show)

getMousePos :: (Double, Double) -> T.Pos -> MouseGridPos
getMousePos (canvasX, canvasY) T.Pos{..} =
    let viewX = floor $ canvasX / _cellWidth
        viewY = floor $ canvasY / _cellHeight
        viewPos = Point viewX viewY

        gridX = _leftXCoord + viewX
        gridY = _topYCoord + viewY
        gridPos = Point gridX gridY
    in MouseGridPos{..}

zoom :: MonadApp m => (ScrollDirection, (Double, Double)) -> m Bool
zoom (scrollDir, evCoords) = do
    MouseGridPos{viewPos = Point viewX viewY} <- getMousePos evCoords <$> getPos
    case scrollDir of
        ScrollDirectionUp   -> modifyCellPos (*2) (+ (viewX `quot` 2)) (+ (viewY `quot` 2)) $> True
        ScrollDirectionDown -> modifyCellPos (/2) (subtract viewX)     (subtract viewY)     $> True
        _                   -> return False

canvasMouseHandler :: MonadApp m
                   => Bool  -- ^ Is this being called from a @buttonPressEvent@?
                   -> (Bool, (Double, Double))  -- ^ whether the mouse button was pressed, and (x, y), of mouse event
                   -> m Bool
canvasMouseHandler fromButtonPress (btnDown, coords) = do
    let isButtonDown = fromButtonPress || btnDown
    pos' <- getPos
    let MouseGridPos{ viewPos = viewP@(Point viewX viewY)
                    , gridPos = gridP@(Point gridX gridY)
                    } = getMousePos coords pos'
    curMode <- getCurrentMode
    if isButtonDown then
        recordNewMousePoint viewP >>= \case
            NoDiff -> return ()
            diff -> case curMode of
                T.DrawMode -> do
                    stnum <- getCurrentDrawingState
                    getOps >>= \Ops{..} ->
                        let newst = states !! stnum
                        in modifyPattern $ curry $ first $ modifyPoint gridP (const newst)
                T.MoveMode -> case diff of
                    -- make pattern-match exhaustive to make GHC happy
                    NoDiff -> error "Unreachable case reached when moving!"
                    NewPoint -> return ()
                    PointDiff (Point dx dy) -> modifyPos $
                        over T.leftXCoord (subtract dx) .
                        over T.topYCoord  (subtract dy)           
                T.SelectMode -> getSelection >>= setSelection . \case
                    Nothing -> Just (gridP, gridP)
                    Just (p1, _) ->
                        -- Restart selection if mouse was lifted, else continue it
                        case diff of
                            NoDiff -> error "Unreachable case reached when selection!"
                            NewPoint -> Nothing
                            PointDiff _ -> Just (p1, gridP)
                T.PastePendingMode oldMode -> do
                    getOps >>= \Ops{..} ->
                        case getClipboard of
                            Nothing -> pure ()
                            Just c -> modifyPattern $ curry $ first $ mergeAtPoint gridP c
                    setPasteSelectionOverlay Nothing
                    setMode oldMode
    else
        case curMode of
            T.PastePendingMode _ -> getOps >>= \Ops{..} ->
                case getClipboard of
                    Nothing -> pure ()
                    Just ccs ->
                        let (w, h) = size ccs in
                            setPasteSelectionOverlay $ Just (gridP, Point (gridX+w-1) (gridY+h-1))
            _ -> return ()
    setCoordsLabel $ "(" <> pack (show $ getCoord gridX) <> "," <> pack (show $ getCoord gridY) <> ")"
    return True
                    
-- | Overwrite all points of one 'Universe' with another, displacing
-- the new 'Universe' to a particular 'Point'.
mergeAtPoint :: Point       -- ^ Where to start overwriting
             -> Universe a  -- ^ The new universe to overwrite with
             -> Universe a  -- ^ The universe to be overwritten
             -> Universe a  -- ^ Result
mergeAtPoint (Point x y) (Universe new) (Universe old) =
    let newAscs = (assocs new) <&> first (\(Point x' y') -> Point (x'+x) (y'+y))
        oldAscs = assocs old
        oldBs   = bounds old
    in Universe $ array oldBs $
        flip fmap oldAscs $ \a@(i, _val) ->
            case lookup i newAscs of
                Nothing -> a
                Just val' -> (i, val')

drawCanvas :: MonadApp m => RenderContext m -> m Bool
drawCanvas ctx = getOps >>= \Ops{..} -> do
    pos <- getPos
    selection <- getSelection
    pasteSelectionOverlay <- getPasteSelectionOverlay
    renderWithContext ctx $ \csize ->
        renderUniverse csize (state2color <$> getPattern) pos selection pasteSelectionOverlay
    return True

renderUniverse :: MonadRender m
               => (Double, Double)
               -> Universe (Double, Double, Double)
               -> T.Pos
               -> Maybe (Point, Point)
               -> Maybe (Point, Point)
               -> m ()
renderUniverse (w, h) grid T.Pos{..} selection pasteSelection = do
{-
This is a bit complex. The universe is finite, so it is possible to move the
viewport to a place which is outside the universe. In this case, only part of
the universe can be displayed, as in the following graphic, where . represents
the universe, solid lines represent the viewport and outlined lines represent
the part of the universe shown on the canvas:
┌──────────────┐
│              │
│              │
│      ╔═══════╡
│      ║.......│.....
│      ║.......│.....
│      ║.......│.....
│      ║.......│.....
└──────╨───────┘.....
        .............
        .............
These parts are represented using Pos, Bounds and Coord values as follows:
┌────────────────────────────────────────────┐
│ Pos{..}               ^                 ^  │
│ viewportBs :: Bounds  |                 |  │
│                       |topRowCoord      |  │
│                       |                 |  │
│                       |                 |  │
│                       |                 |  │
│                       v                 |  │
│                     ╔══════════════════════╡
│ leftColCoord        ║actualBs :: Bounds |  │
│<------------------->║                   |  │
│                     ║                   |  │
│                     ║                   |  │
│                     ║     bottomRowCoord|  │
│                     ║                   |  │
│<--------------------║-------------------|->│
│  rightColCoord      ║                   |  │
│                     ║                   v  │
└─────────────────────╨──────────────────────┘
That is, the viewport is represented using Pos{..} (the third argument) and
viewportBs, and the part of the universe which is shown on the canvas is
represented by actualBs and the various Coord values.
-}
    let viewportBs = Bounds
            { boundsLeft = _leftXCoord
            , boundsTop  = _topYCoord
            , boundsRight  = ceiling $ __ _leftXCoord + (w / _cellWidth)
            , boundsBottom = ceiling $ __ _topYCoord  + (h / _cellHeight)
            }
        (actualBs, clipped) = clipInside grid viewportBs

        leftColCoord  = boundsLeft  actualBs - boundsLeft viewportBs
        rightColCoord = boundsRight actualBs - boundsLeft viewportBs + 1
        topRowCoord    = boundsTop    actualBs - boundsTop viewportBs
        bottomRowCoord = boundsBottom actualBs - boundsTop viewportBs + 1


    for_ (zip [fromIntegral topRowCoord..] clipped) $ \(i, row) ->
        for_ (zip [fromIntegral leftColCoord..] row) $ \(j, (r, g, b)) -> do
            setSourceRGB r g b
            rectangle (j*_cellWidth) (i*_cellHeight) _cellWidth _cellHeight
            fill

    setLineWidth 1.5
    when (_cellHeight > 2) $
        for_ [boundsTop actualBs..boundsBottom actualBs] $ \row -> do
            setSourceRGBA 0 0 0 (getOpacity row)
            let yCoord = (__ $ row - boundsTop viewportBs) * _cellHeight
                xCoordLeft  = (__ leftColCoord ) * _cellWidth
                xCoordRight = (__ rightColCoord) * _cellWidth
            -- Draw a horizontal line at yCoord
            moveTo xCoordLeft  yCoord
            lineTo xCoordRight yCoord
            stroke

    when (_cellWidth > 2) $
        for_ [boundsLeft actualBs..boundsRight actualBs] $ \col -> do
            setSourceRGBA 0 0 0 (getOpacity col)
            let xCoord = (__ $ col - boundsLeft viewportBs) * _cellWidth
                yCoordTop    = (__ topRowCoord   ) * _cellHeight
                yCoordBottom = (__ bottomRowCoord) * _cellHeight
            -- Draw a vertical line at xCoord
            moveTo xCoord yCoordTop
            lineTo xCoord yCoordBottom
            stroke

    drawOverlay selection (0,1,0)
    drawOverlay pasteSelection (0.5,0.5,0)
  where
    getOpacity n = if n `mod` 10 == 0 then 0.3 else 0.15

    -- Clip the second parameter to within the range of the first.
    clipIn :: Ord n => (n,n) -> n -> n
    clipIn (l,h) n | l > n = l
                   | n > h = h
                   | otherwise = n

    drawOverlay overlay (r,g,b) =
        maybeM overlay $ \(Point x1 y1, Point x2 y2) -> do
            let (gw,gh) = size grid
                x1' = __ (getCoord (clipIn (0,gw) x1 - _leftXCoord)) * _cellWidth
                y1' = __ (getCoord (clipIn (0,gh) y1 - _topYCoord))  * _cellHeight
                x2' = __ (getCoord (clipIn (0,gw) x2 +1 - _leftXCoord)) * _cellWidth
                y2' = __ (getCoord (clipIn (0,gh) y2 +1 - _topYCoord))  * _cellHeight
            -- Draw green rectangle for selection
            setSourceRGBA r g b 0.5
            rectangle x1' y1' (x2'-x1') (y2'-y1')
            fill
      where
        maybeM :: Applicative m => Maybe a -> (a -> m ()) -> m ()
        maybeM Nothing  _ = pure ()
        maybeM (Just a) c = c a

-- Short, type-restricted version of fromIntegral for convenience
__ :: Integral a => a -> Double
__ = fromIntegral
