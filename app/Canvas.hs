{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Canvas where

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Foldable (for_)
import Data.Functor (($>))

import Data.Array (array, assocs, bounds)
import Data.Finite (Finite)
import Data.Text (pack)
import Lens.Micro

import CA.Universe
import Control.Monad.App.Class
import qualified Types as T
import Types.Application (defaultPattern, _state2color)

clearPattern :: (Canvas m, EvolutionSettings m, HasRuleConfig n (Finite n) m, Pattern (Finite n) m, SaveRestorePattern m) => m ()
clearPattern = do
    modifyGen $ const 0
    modifyPos $ const T.Pos{_leftXCoord=0,_topYCoord=0,_cellWidth=16,_cellHeight=16}
    resetRestorePattern
    rc <- askRuleConfig
    modifyPattern $ curry $ first $ const (rc ^. defaultPattern)

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

zoom :: Canvas m => (ScrollDirection, (Double, Double)) -> m Bool
zoom (scrollDir, evCoords) = do
    MouseGridPos{viewPos = Point viewX viewY} <- getMousePos evCoords <$> getPos
    case scrollDir of
        ScrollDirectionUp   -> modifyCellPos (*2) (+ (viewX `quot` 2)) (+ (viewY `quot` 2)) $> True
        ScrollDirectionDown -> modifyCellPos (/2) (subtract viewX)     (subtract viewY)     $> True
        _                   -> return False

canvasMouseHandler :: (Canvas m, Clipboard a m, EvolutionSettings m, HasRuleConfig n a m, Modes m, MouseTracking m, Pattern a m)
                   => Bool  -- ^ Is this being called from a @buttonPressEvent@?
                   -> (Bool, (Double, Double))  -- ^ whether the mouse button was pressed, and (x, y), of mouse event
                   -> m Bool
canvasMouseHandler fromButtonPress (btnDown, coords) = do
    let isButtonDown = fromButtonPress || btnDown
    pos' <- getPos
    let MouseGridPos{ viewPos = viewP
                    , gridPos = gridP@(Point gridX gridY)
                    } = getMousePos coords pos'
    curMode <- getCurrentMode
    if isButtonDown then
        recordNewMousePoint viewP >>= \case
            NoDiff -> return ()
            diff -> case curMode of
                T.DrawMode -> do
                    stnum <- getCurrentDrawingState
                    newst <- (!!stnum) <$> states
                    modifyPattern $ curry $ first $ modifyPoint gridP (const newst)
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
                    getClipboard >>= \case
                        Nothing -> pure ()
                        Just c -> modifyPattern $ curry $ first $ mergeAtPoint gridP c
                    setPasteSelectionOverlay Nothing
                    setMode oldMode
    else
        case curMode of
            T.PastePendingMode _ ->
                getClipboard >>= \case
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

drawCanvas :: (Canvas m, HasRuleConfig n (Finite n) m, Pattern (Finite n) m, RenderCanvas m) => RenderContext m -> m Bool
drawCanvas ctx = do
    pos <- getPos
    selection <- getSelection
    pasteSelectionOverlay <- getPasteSelectionOverlay
    state2color <- _state2color <$> askRuleConfig
    _pattern <- getPattern
    renderWithContext ctx $ \csize ->
        renderUniverse csize (state2color <$> _pattern) pos selection pasteSelectionOverlay
    return True

renderUniverse :: MonadRender m
               => (Double, Double)                   -- ^ Size of the screen, in pixels
               -> Universe (Double, Double, Double)  -- ^ Universe to display, with cells being colours represented as @(r,g,b)@ tuples
               -> T.Pos                              -- ^ Position of the viewport
               -> Maybe (Point, Point)               -- ^ Area (if any) which has been selected
               -> Maybe (Point, Point)               -- ^ Area (if any) which is highlighted in pasting mode
               -> m ()
renderUniverse (w, h) grid T.Pos{..} selection pasteSelection = do
{-
This function is complex enough to deserve some explanation. There are
three main inputs to this function to describe the position of the
universe on the screen:

    * The ‘grid’ or ‘universe’, which stores the colours of the cells
      to be displayed.

    * The ‘viewport’, which represents the part of the grid which is
      shown on the screen. The viewport is described in terms of its
      top-left point, represented in cell coordinates (i.e. 1 unit = 1
      cell) relative to the top-left of the grid; its width and height
      are taken to be the width and height of the Cabasa window (given
      as @(w,h)@).

    * The ‘cell size’, which stores the width and height of each cell.

The grid is given as the @grid@ argument; the viewport and cell size
are given together as the @T.Pos{..}@ argument.

Using these, we can then calculate the following values:

    * @viewportBs@ is the result of expressing the viewport as a
      'Bounds' object; this contains the left, top, right and bottom
      coordinates of the viewport, all represented relative to the
      top-left of the grid and expressed in cell coordinates.

    * @actualBs@ is the result of clipping @viewportBs@ so that it
      does not go outside the grid; the corresponding value @clipped@
      is the part of @grid@ which does not go outside the
      viewport. These two values should coincide: the area of @grid@
      which is used in @clipped@ should occupy the coordinates
      expressed in @actualBs@.

    * @leftColCoord@, @rightColCoord@, @topRowCoord@ and
      @bottomRowCoord@ are the x or y coordinates of the leftmost,
      rightmost, topmost and bottommost rows or columns respectively.
      They are expressed in cell coordinates relative to the top-left
      of the SCREEN (not the viewport!).

Perhaps a diagram might help. Here, the dots represent the grid, the
solid box represents the viewport/@viewportBs@, and the inner box
with doubled lines represents @actualBs@:

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

And all the various values relate to this as follows:

┌────────────────────────────────────────────┐
│ viewportBs :: Bounds  ^                 ^  │
│                       |                 |  │
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
        for_ [topRowCoord..bottomRowCoord] $ \row -> do
            setSourceRGBA 0 0 0 (getOpacity $ row + _topYCoord)
            let yCoord = (__ row) * _cellHeight
                xCoordLeft  = (__ leftColCoord ) * _cellWidth
                xCoordRight = (__ rightColCoord) * _cellWidth
            -- Draw a horizontal line at yCoord
            moveTo xCoordLeft  yCoord
            lineTo xCoordRight yCoord
            stroke

    when (_cellWidth > 2) $
        for_ [leftColCoord..rightColCoord] $ \col -> do
            setSourceRGBA 0 0 0 (getOpacity $ col + _leftXCoord)
            let xCoord = (__ col) * _cellWidth
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
    clipIn (l,i) n | l > n = l
                   | n > i = i
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
