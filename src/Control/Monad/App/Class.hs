{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Control.Monad.App.Class
    ( Pattern(..)
    , HasRuleConfig(..)
    , Windows(..)
    , Modes(..)
    , setPastePending
    , PlayThread(..)
    , SaveRestorePattern(..)
    , EvolutionSettings(..)
    , Canvas(..)
    , modifyCellPos
    , MouseTracking(..)
    , Files(..)
    , Paths(..)
    , RenderCanvas(..)
    , Clipboard(..)
    , FileChooserAction(..)
    , Optional
    , PointDiff(..)
    , ScrollDirection(..)
    , MonadRender(..)
    ) where

import Control.Monad.Random.Strict (StdGen, Rand)
import Data.Text (Text)
import GI.Gtk (MessageType(..), ButtonsType(..), ResponseType(..))
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo (Render)

import CA.Universe (Point(..), Universe, Coord(..), Axis(X,Y))
import qualified Cabasa.Types as T

class Monad m => Pattern a m | m -> a where
    -- | Get the current 'Universe'.
    getPattern :: m (Universe a)

    -- | Modify the current 'Universe' using a function which, given
    -- the current 'Universe' and a random 'StdGen', returns an
    -- updated 'Universe' and 'StdGen'.
    modifyPattern :: (Universe a -> StdGen -> (Universe a, StdGen)) -> m ()

class Monad m => HasRuleConfig a m | m -> a where
    -- | Get the current rule config.
    askRuleConfig :: m (T.RuleConfig a)

    -- | Get the list of states for the current rule
    states :: m [a]
    -- | Encode a state value to an 'Int'
    encodeInt :: m (a -> Int)
    -- | Decode a state value from an 'Int'
    decodeInt :: m (Int -> a)

class Monad m => Windows m where
    showMessageDialog :: MessageType -> ButtonsType -> Text -> (ResponseType -> m a) -> m a

    -- | Quit the main window
    mainQuit :: m ()

    -- | Run dialog to select new grid size, and process the new size
    -- if one is returned.
    runGridSizeDialog
        :: (Coord 'X, Coord 'Y)            -- ^ Previous grid size
        -> (Coord 'X -> Coord 'Y -> m ())  -- ^ Callback to process new grid size
        -> m ()

    -- | Display the ‘about’ dialog
    showAboutDialog :: m ()
    -- | Display the user manual
    showUserManual :: m ()

    -- | Show error message with selected text
    showErrorDialog :: Text -> m ()
    -- | Show query dialog
    showQueryDialog
        :: Text
        -> m a  -- Callback if ‘no’ was chosen
    
        -> m a  -- Callback if ‘yes’ was chosen
        -> m a

    -- | Run file dialog to select a pattern file.
    withPatternFileDialog
        :: FileChooserAction i  -- ^ Whether to show a file dialog to open or save
        -> (Optional Text i -> FilePath -> m a)
                                -- ^ Callback with contents (if opening) and path of selected file
        -> m (Maybe a)

class Monad m => Modes m where
    -- | Get the current interaction mode
    getCurrentMode :: m T.InteractionMode
    -- | Set the current interaction mode
    setMode :: T.InteractionMode -> m ()
    -- | Get the state currently being used to draw with
    getCurrentDrawingState :: m Int

-- | Set the current mode as having a paste pending
setPastePending :: Modes m => m ()
setPastePending = getCurrentMode >>= setMode . T.PastePendingMode
    
class Monad m => PlayThread m where
    -- | Toggle the play thread on or off. This thread will repeatedly
    -- perform an action until it is disabled.
    togglePlayThread
        :: m a                 -- ^ Action to perform before enabling play thread
        -> m a                 -- ^ Action to perform repeatedly in play thread
        -> m ()
    -- | Kill the play thread, whether it is currently on or off.
    forceKillThread :: m ()
    -- | Modify the delay between repeats in the play thread (in
    -- microseconds)
    modifyDelay :: (Int -> Int) -> m ()

class Monad m => SaveRestorePattern m where
    -- | Save the current pattern for future restoration
    saveRestorePattern :: m ()
    -- | Restore the last saved pattern if present
    restorePattern :: m ()
    -- | Clear the last saved pattern
    resetRestorePattern :: m ()

class Monad m => EvolutionSettings m where
    -- | Modify the current generation number
    modifyGen :: (Int -> Int) -> m ()
    -- | Set the text of the coordinates label
    setCoordsLabel :: Text -> m ()

class Monad m => Canvas m where
    -- | Get the current selection. Refer to documentation of
    -- '_selection' in @Types.hs@ for details.
    getSelection :: m (Maybe (CA.Universe.Point, CA.Universe.Point))
    -- | Set the current selection.
    setSelection :: Maybe (CA.Universe.Point, CA.Universe.Point) -> m ()

    getColors :: m [(Double, Double, Double)]

    -- | Get the current canvas position
    getPos :: m T.Pos
    -- | Modify the current canvas position
    modifyPos :: (T.Pos -> T.Pos) -> m ()

    -- | A type containing the details of a mouse event
    type MouseEvent m :: *
    -- | Given a mouse event, return whether the mouse had been
    -- pressed during the event, as well as the @x@ and @y@
    -- coordinates of the event
    getMouseEventInfo :: MouseEvent m -> m (Bool, (Double, Double))

    -- | A type containing the details of a scroll event
    type ScrollEvent m :: *
    -- | Given a scroll event, return the scroll direction, as well as
    -- the @x@ and @y@ coordinates of the event
    getScrollEventInfo :: ScrollEvent m -> m (ScrollDirection, (Double, Double))

    -- | Get the current paste overlay
    getPasteSelectionOverlay :: m (Maybe (CA.Universe.Point, CA.Universe.Point))
    -- | Set the current paste overlay
    setPasteSelectionOverlay :: Maybe (CA.Universe.Point, CA.Universe.Point) -> m ()

    -- | Force a canvas redraw. This should be necessary only very
    -- rarely, usually in conjunction with one of the 'GetOps'
    -- methods, as all other relevant canvas manipulation methods
    -- redraw the canvas automatically.
    forceCanvasRedraw :: m ()
    
-- | Modify the position, width and height of the cells.
modifyCellPos :: Canvas m
              => (Double -> Double)     -- ^ Modification to width & height
              -> (Coord 'X -> Coord 'X) -- ^ Modification to left x coordinate
              -> (Coord 'Y -> Coord 'Y) -- ^ Modification to top y coordinate
              -> m ()
modifyCellPos f g h = modifyPos (\T.Pos{..} -> T.Pos (g _leftXCoord) (h _topYCoord) (f _cellWidth) (f _cellHeight))

class Monad m => MouseTracking m where
    -- | Record the point the mouse is currently at. If this is
    -- different to the previously recorded point, returns the
    -- difference between the given point and the last recorded point
    -- as a 'PointDiff'.
    recordNewMousePoint :: Point -> m PointDiff

    -- | Erase the last record of the point the mouse was at. This
    -- will cause 'recordNewMousePoint' to return 'NoPoint' the next
    -- time it is called.
    eraseMousePointRecord :: m ()

class Monad m => Files m where
    listDirectories :: [FilePath] -> m [FilePath]
    readTextFile :: FilePath -> m Text

class Monad m => Paths m where
    -- | Get the name of the current rule
    getCurrentRuleName :: m (Maybe String)

    -- | Get the path of the current pattern, if any
    getCurrentPatternPath :: m (Maybe FilePath)
    -- | Set the path of the current pattern
    setCurrentPatternPath :: FilePath -> m ()
    -- | Write text of pattern to a file
    writePattern :: FilePath -> String -> m ()

class Monad m => RenderCanvas m where
    -- | The type of any context which may be required to render on the canvas.
    -- If no context is required, this will be @()@.
    type RenderContext m :: *
    -- | Given a context and something to render it, render it on the canvas.
    -- The context must be created by the user using the appropriate function(s)
    -- from the GUI library being used.
    renderWithContext
        :: RenderContext m  -- ^ The context
        -> (forall r. MonadRender r => (Double, Double) -> r ())
             -- ^ What to render. The argument is the (width, height) of the canvas.
        -> m ()

class Monad m => Clipboard a m | m -> a where
    -- | Get the current clipboard value, if any.
    getClipboard :: m (Maybe (Universe a))
    -- | Set the current clipboard value.
    setClipboard :: Maybe (Universe a) -> m ()

-- | Action to choose a file. Includes a phantom type variable which
-- expresses whether the action allows the contents to be read
-- (for 'OpenFile') or not (for 'SaveFile', where the file may not
-- exist before it is written). This variable may then be used as
-- a parameter to the 'Optional' type family; this pattern is used in
-- the functions 'withPatternFileDialog' and 'withRuleFileDialog'
-- to output the file contents only if the file is being opened.
data FileChooserAction (includeContents :: Bool) where
    OpenFile :: FileChooserAction 'True
    SaveFile :: FileChooserAction 'False

-- | @Optional p i@ returns @p@ if @i@ is true, otherwise returns
-- @()@.
type family Optional p i where
    Optional p 'True = p
    Optional p 'False = ()

-- | Describes the difference between a previously recorded point
-- and another point. Used as the output of 'recordNewMousePoint'.
data PointDiff = NoDiff    -- ^ No difference
               | NewPoint  -- ^ There was no previously recorded point
               | PointDiff Point
               -- ^ There was a previously recorded point, with the given difference
               -- (i.e. if you have @PointDiff d@, then @prevPoint + d = newPoint@)
               deriving (Show)

-- | Describes the direction of a scroll event.
data ScrollDirection = ScrollDirectionUp | ScrollDirectionDown | ScrollDirectionOther

-- | A more pure class for rendering. The 'Render' monad from @cairo@
-- is great for rendering, but implements 'MonadIO', making purity
-- impossible to maintain. This also means that the 'Render' monad
-- cannot be used with 'MonadApp', as this would provide a way to
-- execute arbitrary IO. So the most important rendering functions
-- have been factored out into 'MonadRender'; this allows purity
-- to be ensured by making functions take an argument of type
-- @forall m. MonadRender m => m ()@ rather than @Render ()@.
class Monad m => MonadRender m where
   setSourceRGB :: Double -> Double -> Double -> m ()
   setSourceRGBA :: Double -> Double -> Double -> Double -> m ()
   rectangle :: Double -> Double -> Double -> Double -> m ()
   stroke :: m ()
   fill :: m ()
   setLineWidth :: Double -> m ()
   moveTo :: Double -> Double -> m ()
   lineTo :: Double -> Double -> m ()

instance MonadRender Render where
   setSourceRGB = C.setSourceRGB 
   setSourceRGBA = C.setSourceRGBA 
   rectangle = C.rectangle 
   stroke = C.stroke 
   fill = C.fill 
   setLineWidth = C.setLineWidth 
   moveTo = C.moveTo
   lineTo = C.lineTo 
