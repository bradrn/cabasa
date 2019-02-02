{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Canvas (addCanvasHandlers) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.IORef

import Graphics.Rendering.Cairo hiding (clip)
import Graphics.UI.Gtk hiding (Point, rectangle, cellWidth, cellHeight)
import Lens.Micro

import CA.Universe
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
        pos'       <- liftIO $ readIORef (app ^. T.pos)
        selection' <- liftIO $ readIORef (app ^. T.selection)
        renderUniverse canvas' currentPattern' pos' selection'

    _ <- canvas' `on` buttonPressEvent  $ canvasMouseHandler True app
    _ <- canvas' `on` motionNotifyEvent $ canvasMouseHandler False app
    _ <- canvas' `on` buttonReleaseEvent $ liftIO $ writeIORef (app ^. T.lastPoint) Nothing $> True

    _ <- canvas' `on` scrollEvent $ zoom app

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

data MouseGridPos = MouseGridPos
    { gridPos :: Point
      -- ^ The (x,y) coordinates of the cursor position, in grid
      -- coordinates, relative to the top-left of the grid
    , viewPos :: Point
      -- ^ Same as 'gridPos', but relative to the top-left of the
      -- Cabasa grid view
    } deriving (Show)

getMousePos :: HasCoordinates t => T.Pos -> EventM t MouseGridPos
getMousePos T.Pos{..} = do
    (canvasX, canvasY) <- eventCoordinates
    let viewX = floor $ canvasX / _cellWidth
        viewY = floor $ canvasY / _cellHeight
        viewPos = Point viewX viewY

        gridX = _leftXCoord + viewX
        gridY = _topYCoord + viewY
        gridPos = Point gridX gridY
    return MouseGridPos{..}

modifyCellPos :: (Double -> Double)     -- ^ Modification to width & height
              -> (Coord 'X -> Coord 'X) -- ^ Modification to left x coordinate
              -> (Coord 'Y -> Coord 'Y) -- ^ Modification to top y coordinate
              -> T.Application -> IO ()
modifyCellPos fCell fX fY app = do
    modifyIORef (app ^. T.pos) $ over T.cellWidth  fCell
                               . over T.cellHeight fCell
                               . over T.leftXCoord fX
                               . over T.topYCoord  fY
    widgetQueueDraw (app ^. T.canvas)

zoom :: T.Application -> EventM EScroll Bool
zoom app = do
    MouseGridPos{viewPos = Point viewX viewY} <- getMousePos =<< liftIO (readIORef (app ^. T.pos))
    eventScrollDirection >>= \case
        ScrollUp   -> liftIO $ modifyCellPos (*2) (+ (viewX `quot` 2)) (+ (viewY `quot` 2)) app $> True
        ScrollDown -> liftIO $ modifyCellPos (/2) (subtract viewX)     (subtract viewY)     app $> True
        _          -> return False

canvasMouseHandler :: (HasCoordinates t, HasModifier t)
                   => Bool  -- ^ Is this being called from a @buttonPressEvent@?
                   -> T.Application -> EventM t Bool
canvasMouseHandler fromButtonPress app = do
    ms <- eventModifierMouse
    let isButtonDown = fromButtonPress || (Button1 `elem` ms)
    pos' <- liftIO $ readIORef (app ^. T.pos)
    MouseGridPos{ viewPos = viewP@(Point viewX viewY)
                , gridPos = gridP@(Point gridX gridY)
                } <- getMousePos pos'
    liftIO $ do
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
                T.SelectMode -> readIORef (app ^. T.selection) >>= \case
                    Nothing ->
                        writeIORef (app ^. T.selection) (Just (gridP, gridP))
                    Just (p1, _) ->
                        writeIORef (app ^. T.selection) $
                            -- Restart selection if mouse was lifted, else continue it
                            case lastPoint' of
                                Nothing -> Nothing
                                Just _  -> Just (p1, gridP)
            writeIORef (app ^. T.lastPoint) $ Just viewP
            widgetQueueDraw (app ^. T.canvas)
        labelSetText (app ^. T.coordsLbl) $
            "(" ++ show (getCoord gridX) ++ "," ++ show (getCoord gridY) ++ ")"
    return True

renderUniverse :: WidgetClass widget => widget -> Universe (Double, Double, Double) -> T.Pos -> Maybe (Point, Point) -> Render ()
renderUniverse canvas grid T.Pos{..} selection = do
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

    maybeM selection $ \(Point x1 y1, Point x2 y2) -> do
        let (gw,gh) = size grid
            x1' = __ (getCoord (clipIn (0,gw) x1 - _leftXCoord)) * _cellWidth
            y1' = __ (getCoord (clipIn (0,gh) y1 - _topYCoord))  * _cellHeight
            x2' = __ (getCoord (clipIn (0,gw) x2 - _leftXCoord)) * _cellWidth
            y2' = __ (getCoord (clipIn (0,gh) y2 - _topYCoord))  * _cellHeight
        -- Draw green rectangle for selection
        setSourceRGBA 0 1 0 0.5
        rectangle x1' y1' (x2'-x1') (y2'-y1')
        fill
  where
    getOpacity n = if n `mod` 10 == 0 then 0.3 else 0.15

    maybeM :: Applicative m => Maybe a -> (a -> m ()) -> m ()
    maybeM Nothing  _ = pure ()
    maybeM (Just a) c = c a

    -- Clip the second parameter to within the range of the first.
    clipIn :: Ord n => (n,n) -> n -> n
    clipIn (l,h) n | l > n = l
                   | n > h = h
                   | otherwise = n

-- Short, type-restricted version of fromIntegral for convenience
__ :: Integral a => a -> Double
__ = fromIntegral
