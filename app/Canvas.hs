{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Canvas (addCanvasHandlers) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Int (Int32)
import Data.IORef
import Foreign.Ptr (castPtr)

import Control.Monad.Trans.Reader (runReaderT)
import Data.Array (array, assocs, bounds)
import Data.GI.Base.Attributes
import Data.Text (pack)
import Graphics.Rendering.Cairo hiding (clip)
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import qualified GI.Cairo
import GI.Gdk hiding (Point)
import GI.Gtk
import Lens.Micro

import CA.Universe
import Utils
import qualified Types as T

addCanvasHandlers :: T.Application -> IO ()
addCanvasHandlers app = do
    let canvas' = app ^. T.canvas  -- because we use this field so much
    widgetAddEvents canvas'
        [ EventMaskButtonPressMask
        , EventMaskButtonReleaseMask
        , EventMaskButtonMotionMask
        , EventMaskPointerMotionMask
        , EventMaskScrollMask
        ]

    _ <- on canvas' #draw $ \context -> T.withState app $ \state -> do
        let renderFn = state ^. T.state2color
            currentPattern' = renderFn <$> state ^. T.currentPattern . _1
        pos'       <- liftIO $ readIORef (app ^. T.pos)
        selection' <- liftIO $ readIORef (app ^. T.selection)
        pasteSelectionOverlay' <- liftIO $ readIORef (app ^. T.pasteSelectionOverlay)
        -- Carry out the rendering
        renderWithContext context $
            renderUniverse canvas' currentPattern' pos' selection' pasteSelectionOverlay'
        return True

    _ <- on canvas' #buttonPressEvent  $ canvasMouseHandler True app
    _ <- on canvas' #motionNotifyEvent $ canvasMouseHandler False app
    _ <- on canvas' #buttonReleaseEvent $ \_ -> liftIO $ writeIORef (app ^. T.lastPoint) Nothing $> True

    _ <- on canvas' #scrollEvent $ zoom app

    _ <- on (app ^. T.clearPattern) #activate $ do
        modifyGeneration app (const 0)
        T.modifyStateM app $ \state -> do
            let defGrid = state ^. T.defaultPattern
                defPos = T.Pos{_leftXCoord=0,_topYCoord=0,_cellWidth=16,_cellHeight=16}
            writeIORef (app ^. T.pos) defPos
            return $ state & T.saved .~ Nothing
                           & (T.currentPattern . _1) .~ defGrid
        widgetQueueDraw canvas'

    _ <- on (app ^. T.clearSelection) #activate $
        writeIORef (app ^. T.selection) Nothing >> widgetQueueDraw canvas'

    return ()
  where
    -- from https://github.com/haskell-gi/haskell-gi/blob/36e4c4fb0df9e80d3c9b2f5999b65128e20317fb/examples/advanced/Cairo.hs#L297
    renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
    renderWithContext ct r = withManagedPtr ct $ \p ->
                            runReaderT (runRender r) (Cairo (castPtr p))

data MouseGridPos = MouseGridPos
    { gridPos :: Point
      -- ^ The (x,y) coordinates of the cursor position, in grid
      -- coordinates, relative to the top-left of the grid
    , viewPos :: Point
      -- ^ Same as 'gridPos', but relative to the top-left of the
      -- Cabasa grid view
    } deriving (Show)

getMousePos :: ( AttrGetC i1 ev "x" Double
               , AttrGetC i2 ev "y" Double
               ) => ev -> T.Pos -> IO MouseGridPos
getMousePos ev T.Pos{..} = do
    (canvasX, canvasY) <- (,) <$> get ev #x <*> get ev #y
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

zoom :: T.Application -> EventScroll -> IO Bool
zoom app ev = do
    MouseGridPos{viewPos = Point viewX viewY} <- getMousePos ev =<< liftIO (readIORef (app ^. T.pos))
    getEventScrollDirection ev >>= \case
        ScrollDirectionUp   -> liftIO $ modifyCellPos (*2) (+ (viewX `quot` 2)) (+ (viewY `quot` 2)) app $> True
        ScrollDirectionDown -> liftIO $ modifyCellPos (/2) (subtract viewX)     (subtract viewY)     app $> True
        _                   -> return False

canvasMouseHandler :: ( AttrGetC i1 ev "state" [ModifierType]
                      , AttrGetC i2 ev "x" Double
                      , AttrGetC i3 ev "y" Double
                      )
                   => Bool  -- ^ Is this being called from a @buttonPressEvent@?
                   -> T.Application
                   -> ev -> IO Bool
canvasMouseHandler fromButtonPress app ev = do
    ms <- get ev #state
    let isButtonDown = fromButtonPress || (ModifierTypeButton1Mask `elem` ms)
    pos' <- liftIO $ readIORef (app ^. T.pos)
    MouseGridPos{ viewPos = viewP@(Point viewX viewY)
                , gridPos = gridP@(Point gridX gridY)
                } <- getMousePos ev pos'
    curMode <- liftIO $ readIORef (app ^. T.currentMode)
    liftIO $ do
        lastPoint' <- readIORef (app ^. T.lastPoint)
        if (isButtonDown && (maybe True (viewP /=) lastPoint')) then do
            case curMode of
                T.DrawMode -> do
                    stnum <- comboBoxGetActiveIter (app ^. T.curstate) >>= \case
                        (False, _) -> return 0
                        (True, iter) -> treeModelGetValue (app ^. T.curstatem) iter 0 >>= fromGValue @Int32
                    T.modifyState app $ \state ->
                        let newst = (state ^. T.states) !! fromIntegral stnum
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
                T.PastePendingMode oldMode -> do
                    T.modifyState app $ \state ->
                        case state ^. T.clipboardContents of
                            Nothing -> state
                            Just c -> state & (T.currentPattern . _1) %~ mergeAtPoint gridP c
                    writeIORef (app ^. T.pasteSelectionOverlay) Nothing
                    writeIORef (app ^. T.currentMode) oldMode
            writeIORef (app ^. T.lastPoint) $ Just viewP
            widgetQueueDraw (app ^. T.canvas)
        else
            case curMode of
                T.PastePendingMode _ ->
                    T.withState app $ \state ->
                        case (state ^. T.clipboardContents) of
                            Nothing -> pure ()
                            Just ccs -> do
                                let (w, h) = size ccs
                                writeIORef (app ^. T.pasteSelectionOverlay) $
                                    Just (gridP, Point (gridX+w-1) (gridY+h-1))
                                widgetQueueDraw (app ^. T.canvas)
                _ -> return ()
        labelSetText (app ^. T.coordsLbl) $
            "(" <> pack (show $ getCoord gridX) <> "," <> pack (show $ getCoord gridY) <> ")"
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

renderUniverse :: IsWidget widget
               => widget
               -> Universe (Double, Double, Double)
               -> T.Pos
               -> Maybe (Point, Point)
               -> Maybe (Point, Point)
               -> Render ()
renderUniverse canvas grid T.Pos{..} selection pasteSelection = do
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
