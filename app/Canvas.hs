{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Canvas (addCanvasHandlers) where

import Data.Foldable (for_)
import Data.IORef

import Graphics.Rendering.Cairo hiding (clip)
import Graphics.UI.Gtk hiding (Point, rectangle, cellWidth, cellHeight)
import Lens.Micro

import CA hiding (pos)
import Types

addCanvasHandlers :: Application -> (Application -> (Int -> Int) -> IO ())-> IO ()
addCanvasHandlers app modifyGeneration = do
    let canvas' = app ^. canvas  -- because we use this field so much
    widgetAddEvents canvas' [ButtonPressMask, ButtonReleaseMask, ButtonMotionMask, ScrollMask]

    _ <- canvas' `on` draw $ withState app $ \state -> do
        let renderFn = state ^. state2color
            currentPattern' = renderFn <$> state ^. currentPattern . _1
        pos' <- liftIO $ readIORef (app ^. pos)
        renderUniverse canvas' currentPattern' pos'

    _ <- canvas' `on` buttonPressEvent  $ canvasMouseHandler app
    _ <- canvas' `on` motionNotifyEvent $ canvasMouseHandler app
    _ <- canvas' `on` buttonReleaseEvent $ liftIO $ writeIORef (app ^. lastPoint) Nothing $> True

    let modifyCellSize :: (Double -> Double) -> IO ()
        modifyCellSize f = do
            modifyIORef (app ^. pos) $ over cellWidth  f
                                     . over cellHeight f
            widgetQueueDraw canvas'
    _ <- canvas' `on` scrollEvent $ eventScrollDirection >>= \case
        ScrollUp   -> liftIO $ modifyCellSize (*2) $> True
        ScrollDown -> liftIO $ modifyCellSize (/2) $> True
        _          -> return False

    _ <- (app ^. clearPattern) `on` menuItemActivated $ do
        modifyGeneration app (const 0)
        modifyStateM app $ \state -> do
            let defGrid = state ^. defaultPattern
                defPos = Pos{_leftXCoord=0,_topYCoord=0,_cellWidth=16,_cellHeight=16}
            writeIORef (app ^. pos) defPos
            return $ state & saved .~ Nothing
                           & (currentPattern . _1) .~ defGrid
        widgetQueueDraw canvas'

    return ()

canvasMouseHandler :: HasCoordinates t => Application -> EventM t Bool
canvasMouseHandler app = do
    (canvasX, canvasY) <- eventCoordinates
    liftIO $ do
        pos'@Pos{..} <- readIORef (app ^. pos)
        let viewX = floor $ canvasX / _cellWidth
            viewY = floor $ canvasY / _cellHeight
            viewP = Point viewX viewY
            gridP = Point (_leftXCoord + viewX) (_topYCoord + viewY)
        lastPoint' <- readIORef (app ^. lastPoint)
        when (maybe True (/= viewP) lastPoint') $ do
            readIORef (app ^. currentMode) >>= \case
                DrawMode -> do
                    stnum <- comboBoxGetActiveIter (app ^. curstate) >>= \case
                        Nothing -> return 0
                        Just iter -> listStoreGetValue (app ^. curstatem) $ listStoreIterToIndex iter
                    modifyState app $ \state ->
                        let newst = (state ^. states) !! stnum
                        in state & (currentPattern . _1) %~ modifyPoint gridP (const newst)
                MoveMode -> case lastPoint' of
                    Nothing -> return ()
                    Just (Point lastX lastY) -> writeIORef (app ^. pos) $
                        pos' & over leftXCoord (+ (lastX-viewX))
                             & over topYCoord  (+ (lastY-viewY))
            writeIORef (app ^. lastPoint) $ Just viewP
            widgetQueueDraw (app ^. canvas)
    return True

renderUniverse :: WidgetClass widget => widget -> Universe (Double, Double, Double) -> Pos -> Render ()
renderUniverse canvas grid Pos{..} = do
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
    w <- liftIO $ __ <$> widgetGetAllocatedWidth canvas
    h <- liftIO $ __ <$> widgetGetAllocatedHeight canvas
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


    for_ (zip [fromIntegral leftColCoord..] clipped) $ \(i, row) ->
        for_ (zip [fromIntegral topRowCoord..] row) $ \(j, (r, g, b)) -> do
            setSourceRGB r g b
            rectangle (i*_cellWidth) (j*_cellHeight) _cellWidth _cellHeight
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
  where
    getOpacity n = if n `mod` 10 == 0 then 0.3 else 0.15

-- Short, type-restricted version of fromIntegral for convenience
__ :: Integral a => a -> Double
__ = fromIntegral
