{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

module Control.Monad.App.Class
    ( GetOps(..)
    , Settings(..)
    , getSetting'
    , Windows(..)
    , showSettingsDialogAndSave
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
    , Ops(..)
    , FileChooserAction(..)
    , Optional
    , PointDiff(..)
    , ScrollDirection(..)
    , MonadRender(..)
    ) where

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Proxy
import GHC.TypeLits (KnownNat, natVal)

import Control.Monad.Random.Strict (StdGen, Rand)
import qualified Data.Finite as F
import Data.Text (Text, pack)
import GI.Gtk (MessageType(..), ButtonsType(..), ResponseType(..))
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo (Render)
import Lens.Micro (Traversal', Lens', _Just, (^.))
import System.FilePath (takeBaseName)

import CA.ALPACA (AlpacaData(..), runALPACA)
import CA.Universe (Point(..), Universe, Coord(..), Axis(X,Y), CARuleA)
import qualified Types as T
import qualified Types.Application as T

class Monad m => GetOps m where
    -- | In Cabasa, the state type of the cells in the CA is allowed
    -- to be any Haskell type. Thus the 'Universe' is stored under an
    -- existential (here denoted @exists a. Universe a@ for clarity)
    -- to state that, although the universe does store a specific cell
    -- type, there is no way of knowing exactly which type this is.
    --
    -- Although this solution works well, it makes it difficult to
    -- manipulate functions which require the usage of existentials,
    -- as none of the type variables match up: for instance, if
    -- @getUniverse :: (exists a. Universe a)@, and
    -- @encodeInt :: (exists a. a -> Int)@, the two @a@s may well
    -- refer to different types. For this reason, functions which
    -- require the use of this existential type variable cannot be
    -- stored directly in the 'MonadApp' typeclass. Instead, they
    -- are stored in the 'Ops' record, which contains an existentially
    -- quantified type variable. @getOps@ is then used to access
    -- an 'Ops' value and hence call these existential operations.
    -- Typical usage will be something like
    -- @getOps >>= \\Ops{..} -> doStuffWithOpsBroughtIntoContext@.
    -- (Note that this uses the @RecordWildCards@ extention to
    -- automatically bring every operation in @Ops@ into scope.)
    getOps :: m (Ops m)

class Monad m => Settings m where
    saveSettings :: T.Settings -> m ()
    getSetting :: Traversal' T.Settings a -> m a

getSetting' :: Settings m => Lens' T.Settings (Maybe a) -> m a
getSetting' s = getSetting (s . _Just)

class Monad m => Windows m where
    showMessageDialog :: MessageType -> ButtonsType -> Text -> (ResponseType -> m a) -> m a

    -- | Quit the main window
    mainQuit :: m ()
    -- | Delete the ‘edit stylesheet’ window
    stylesheetWindowDelete :: m ()

    -- | Run dialog to select new grid size, and process the new size
    -- if one is returned.
    runGridSizeDialog
        :: (Coord 'X, Coord 'Y)            -- ^ Previous grid size
        -> (Coord 'X -> Coord 'Y -> m ())  -- ^ Callback to process new grid size
        -> m ()
    -- | Display the ‘settings’ dialog, with a callback to process the
    -- user’s newly chosen settings
    showSettingsDialog :: (T.Settings -> m ()) -> m ()

    -- | Display the ‘edit sheet’ window
    showEditSheetWindow :: m ()

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
    -- | Run file dialog to select a CSS stylesheet file.
    withCSSFileDialog
        :: FileChooserAction i  -- ^ Whether to show a file dialog to open or save
        -> (Optional Text i -> FilePath -> m a)
                                -- ^ Callback with contents (if opening) and path of selected file
        -> m (Maybe a)

showSettingsDialogAndSave :: (Settings m, Windows m) => m ()
showSettingsDialogAndSave = showSettingsDialog saveSettings

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

    -- | Get the text of the current stylesheet (or an empty string if there is none)
    getStylesheetText :: m Text
    -- | Get the path of the current stylesheet, if any
    getCurrentStylesheetPath :: m (Maybe FilePath)
    -- | Write text of stylesheet to a file
    writeSheet :: FilePath -> Text -> m ()
    -- | Set the stylesheet window to display the text of a specific stylesheet
    setStylesheetWindowStylesheet :: String -> m ()

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

-- | Existentially-quantified functions and accessors. These should
-- really be part of 'MonadApp', but since they use existentials, this
-- would make them unusable; see the documentation for 'getOps' for
-- more details.
data Ops m = forall a. Ops
    { -- | Get the current 'Universe'.
      getPattern :: Universe a

      -- | Modify the current 'Universe' using a function which, given
      -- the current 'Universe' and a random 'StdGen', returns an
      -- updated 'Universe' and 'StdGen'.
    , modifyPattern :: (Universe a -> StdGen -> (Universe a, StdGen)) -> m ()

      -- | Get the current CA rule.
    , getRule :: Point -> Universe a -> Rand StdGen a

      -- | Get the current clipboard value, if any.
    , getClipboard :: Maybe (Universe a)

      -- | Set the current clipboard value.
    , setClipboard :: Maybe (Universe a) -> m ()

      -- | Given a 'Point', get the default cell value associated
      -- with it.
    , defaultVal :: Point -> a

      -- | Get the default 'Universe', used when a pattern is cleared.
      -- This is computed from 'defaultVal'.
    , defaultPattern :: Universe a

      -- | Get the list of states for the current rule
    , states :: [a]

      -- | Encode a state value to an 'Int'
    , encodeInt :: a -> Int
      -- | Decode a state value from an 'Int'
    , decodeInt :: Int -> a

      -- | Get the (r,g,b) triplet corresponding to a state
    , state2color :: a -> (Double, Double, Double)
      -- | Set the function mapping states to colours
    , setState2Color :: (a -> (Double, Double, Double)) -> m ()

      -- | Get the name of a state, as a 'String'
    , getName :: a -> Maybe String
    }

-- | Action to choose a file. Includes a phantom type variable which
-- expresses whether the action allows the contents to be read
-- (for 'OpenFile') or not (for 'SaveFile', where the file may not
-- exist before it is written). This variable may then be used as
-- a parameter to the 'Optional' type family; this pattern is used in
-- the functions 'withPatternFileDialog', 'withRuleFileDialog' and
-- 'withCSSFileDialog' to output the file contents only if the file
-- is being opened.
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
