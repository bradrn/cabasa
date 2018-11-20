{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Canvas (addCanvasHandlers) where

import Data.Foldable (for_)
import Data.IORef

import Graphics.Rendering.Cairo hiding (clip)
import Graphics.UI.Gtk hiding (Point, rectangle, cellWidth, cellHeight)
import Lens.Micro

import CA hiding (pos)
import Utils
import qualified Types as T

addCanvasHandlers :: T.Application -> IO ()
addCanvasHandlers app = do
    let canvas' = app ^. T.canvas  -- because we use this field so much
    widgetAddEvents canvas'
        [ ButtonPressMask
        , ButtonReleaseMask
        , ButtonMotionMask
        , PointerMotionMask
        , ScrollMask
        ]

    _ <- canvas' `on` draw $ T.withState app $ \state -> do
        let renderFn = state ^. T.state2color
            currentPattern' = renderFn <$> state ^. T.currentPattern . _1
        pos' <- liftIO $ readIORef (app ^. T.pos)
        renderUniverse canvas' currentPattern' pos'

    _ <- canvas' `on` buttonPressEvent  $ canvasMouseHandler True app
    _ <- canvas' `on` motionNotifyEvent $ canvasMouseHandler False app
    _ <- canvas' `on` buttonReleaseEvent $ liftIO $ writeIORef (app ^. T.lastPoint) Nothing $> True

    let modifyCellSize :: (Double -> Double) -> IO ()
        modifyCellSize f = do
            modifyIORef (app ^. T.pos) $ over T.cellWidth  f
                                       . over T.cellHeight f
            widgetQueueDraw canvas'
    _ <- canvas' `on` scrollEvent $ eventScrollDirection >>= \case
        ScrollUp   -> liftIO $ modifyCellSize (*2) $> True
        ScrollDown -> liftIO $ modifyCellSize (/2) $> True
        _          -> return False

    _ <- (app ^. T.clearPattern) `on` menuItemActivated $ do
        modifyGeneration app (const 0)
        T.modifyStateM app $ \state -> do
            let defGrid = state ^. T.defaultPattern
                defPos = T.Pos{_leftXCoord=0,_topYCoord=0,_cellWidth=16,_cellHeight=16}
            writeIORef (app ^. T.pos) defPos
            return $ state & T.saved .~ Nothing
                           & (T.currentPattern . _1) .~ defGrid
        widgetQueueDraw canvas'

    return ()

canvasMouseHandler :: (HasCoordinates t, HasModifier t)
                   => Bool  -- ^ Is this being called from a @buttonPressEvent@?
                   -> T.Application -> EventM t Bool
canvasMouseHandler fromButtonPress app = do
    (canvasX, canvasY) <- eventCoordinates
    ms <- eventModifierMouse
    let isButtonDown = fromButtonPress || (Button1 `elem` ms)
    liftIO $ do
        pos'@T.Pos{..} <- readIORef (app ^. T.pos)
        let viewX = floor $ canvasX / _cellWidth
            viewY = floor $ canvasY / _cellHeight
            viewP = Point viewX viewY

            gridX = _leftXCoord + viewX
            gridY = _topYCoord + viewY
            gridP = Point gridX gridY
        lastPoint' <- readIORef (app ^. T.lastPoint)
        when (isButtonDown && (maybe True (/= viewP) lastPoint')) $ do
            readIORef (app ^. T.currentMode) >>= \case
                T.DrawMode -> do
                    stnum <- comboBoxGetActiveIter (app ^. T.curstate) >>= \case
                        Nothing -> return 0
                        Just iter -> listStoreGetValue (app ^. T.curstatem) $ listStoreIterToIndex iter
                    T.modifyState app $ \state ->
                        let newst = (state ^. T.states) !! stnum
                        in state & (T.currentPattern . _1) %~ modifyPoint gridP (const newst)
                T.MoveMode -> case lastPoint' of
                    Nothing -> return ()
                    Just (Point lastX lastY) -> writeIORef (app ^. T.pos) $
                        pos' & over T.leftXCoord (+ (lastX-viewX))
                             & over T.topYCoord  (+ (lastY-viewY))
            writeIORef (app ^. T.lastPoint) $ Just viewP
            widgetQueueDraw (app ^. T.canvas)
        labelSetText (app ^. T.coordsLbl) $
            "(" ++ show (getCoord gridX) ++ "," ++ show (getCoord gridY) ++ ")"
    return True

renderUniverse :: WidgetClass widget => widget -> Universe (Double, Double, Double) -> T.Pos -> Render ()
renderUniverse canvas grid T.Pos{..} = do
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
