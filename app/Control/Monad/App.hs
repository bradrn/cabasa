{-# OPTIONS_GHC -Werror=missing-fields -Werror=missing-methods #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Control.Monad.App
    ( MonadApp(..)
    , Ops(..)
    , FileChooserAction(..)
    , Optional
    , PointDiff(..)
    , App
    , runApp
    , MonadRender(..)
    ) where

import Control.Applicative ((<|>))
import Control.Concurrent (killThread, forkIO, threadDelay)
import Data.Int (Int32)
import Data.IORef
import Data.List (find)
import Data.Maybe (fromMaybe)
import Foreign.Ptr (castPtr)

import Control.Monad.Random.Strict (StdGen, Rand)
import Control.Monad.Reader
import Data.Text (Text, pack, uncons)
import qualified Data.Text.IO as TIO
import qualified GI.Cairo
import Data.GI.Gtk.Threading (postGUIASync)
import GI.Gtk hiding (FileChooserAction)
import qualified GI.Gtk  -- for FileChooserAction
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo (Render)
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Lens.Micro hiding (set)
import System.FilePath ((</>), (-<.>), takeBaseName, takeExtension)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Process (callCommand)

import CA.Universe (Point(..), Universe, Coord(..), Axis(X,Y))
import qualified Utils as U
import Settings (getSetting')
import qualified SettingsDialog as S
import qualified Types as T
import Paths_cabasa

{-|

'MonadApp' is a monad containing all the ‘primitive’ operations
necessary to implement event handlers in Cabasa. The intention is that
every event handler should be defined as a 'MonadApp', rather than
simply implementing them in 'IO'. 'MonadApp' has several advantages
compared to using 'IO':

    * An application written entirely in 'IO' has no limits on what
      can be done by as part of a event handler. For instance, any
      function could read any file or mutate an arbitrary variable.
      This is generally considered to be a Bad Thing. By using a
      custom typeclass to encapsulate the available types of
      ‘primitive’ actions available, limits can be placed on exactly
      which side-effecting actions can be used. For instance, file
      reading should be restricted to opening a file selected by the
      user, so is included within the file-opening functions (such as
      'withRuleFileDialog'). Unlimited mutation is controlled by not
      including too many accessor functions to directly access mutable
      variables.

    * 'MonadApp' allows for more modularity compared to 'IO': the same
      event handler can be used across multiple backends, and porting
      to another backend should require only writing a new 'MonadApp'
      instance and adding another @main@ function. This is
      particularly useful for testing, or for implementing Cabasa in
      another GUI framework if necessary.

Since 'MonadApp' contains all the side-effecting actions used in
Cabasa, it will often be necessary to add new methods to 'MonadApp'
whenever new functionality is added to Cabasa. For this reason it is
useful to know what sort of methods 'MonadApp' should contain. There
is no clear criterion for this, but in general, methods in 'MonadApp'
should satisfy the following points, roughly in order of priority:

    * They should be __orthogonal__: if a method is in 'MonadApp', it
      should not be used by any other methods in 'MonadApp'.

    * If two side-effecting actions are consistently used together,
      they should be __combined__ into a single 'MonadApp' method.

    * They should __not correspond to a single 'IO' action__, since
      this defeats the purpose of using a separate typeclass rather
      than using 'IO'

    * They should be __reusable__: rather than adding the whole
      side-effecting action as a single method, it is preferable
      to break it up into smaller methods which could be reused in
      multiple contexts.

    * They should __not contain large amounts of logic__: 'MonadApp'
      is meant to be a typeclass for side-effecting computations, so
      logic should be placed in GUI event handlers rather than in
      'MonadApp'

Note that this is just a rough guide though: just about any
side-effecting action may be placed in 'MonadApp' if it seems OK and
will make the code simpler.
-}
class Monad m => MonadApp m where
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

    -- | Get the current interaction mode
    getCurrentMode :: m T.InteractionMode
    -- | Set the current interaction mode
    setMode :: T.InteractionMode -> m ()
    -- | Get the state currently being used to draw with
    getCurrentDrawingState :: m Int

    -- | Toggle the play thread on or off. This thread will repeatedly
    -- perform an action until it is disabled.
    togglePlayThread
        :: m a                 -- ^ Action to perform before enabling play thread
        -> m a                 -- ^ Action to perform repeatedly in play thread
        -> m ()
    -- | Kill the play thread, whether it is currently on or off.
    forceKillThread :: m ()

    -- | Save the current pattern for future restoration
    saveRestorePattern :: m ()
    -- | Restore the last saved pattern if present
    restorePattern :: m ()
    -- | Clear the last saved pattern
    resetRestorePattern :: m ()

    -- | Modify the current generation number
    modifyGen :: (Int -> Int) -> m ()
    -- | Modify the delay between steps (in microseconds)
    modifyDelay :: (Int -> Int) -> m ()
    -- | Set the text of the coordinates label
    setCoordsLabel :: Text -> m ()

    -- | Get the current selection. Refer to documentation of
    -- '_selection' in @Types.hs@ for details.
    getSelection :: m (Maybe (CA.Universe.Point, CA.Universe.Point))
    -- | Set the current selection.
    setSelection :: Maybe (CA.Universe.Point, CA.Universe.Point) -> m ()

    -- | Get the current canvas position
    getPos :: m T.Pos
    -- | Modify the current canvas position
    modifyPos :: (T.Pos -> T.Pos) -> m ()

    -- | Modify the position, width and height of the cells.
    --
    -- (Really this is just equivalent to 'modifyPos', but is useful
    -- enough that it’s worth having two functions to do the same
    -- thing.)
    modifyCellPos :: (Double -> Double)     -- ^ Modification to width & height
                  -> (Coord 'X -> Coord 'X) -- ^ Modification to left x coordinate
                  -> (Coord 'Y -> Coord 'Y) -- ^ Modification to top y coordinate
                  -> m ()

    -- | Record the point the mouse is currently at. If this is
    -- different to the previously recorded point, returns the
    -- difference between the given point and the last recorded point
    -- as a 'PointDiff'.
    recordNewMousePoint :: Point -> m PointDiff

    -- | Get the current paste overlay
    getPasteSelectionOverlay :: m (Maybe (CA.Universe.Point, CA.Universe.Point))
    -- | Set the current paste overlay
    setPasteSelectionOverlay :: Maybe (CA.Universe.Point, CA.Universe.Point) -> m ()
    
    -- | Run dialog to select new grid size, and process the new size
    -- if one is returned.
    runGridSizeDialog
        :: (Coord 'X, Coord 'Y)            -- ^ Previous grid size
        -> (Coord 'X -> Coord 'Y -> m ())  -- ^ Callback to process new grid size
        -> m ()
    -- | Display the ‘settings’ dialog
    showSettingsDialog :: m ()

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
    -- | Run file dialog to select a rule file.
    withRuleFileDialog
        :: FileChooserAction i  -- ^ Whether to show a file dialog to open or save
        -> Maybe T.Rule         -- ^ File format of rule to choose
        -> (Maybe T.Rule -> Optional Text i -> FilePath -> m a)
                                -- ^ Callback with chosen rule type, contents (if opening) and path of selected file
        -> m (Maybe a)
    -- | Run file dialog to select a CSS stylesheet file.
    withCSSFileDialog
        :: FileChooserAction i  -- ^ Whether to show a file dialog to open or save
        -> (Optional Text i -> FilePath -> m a)
                                -- ^ Callback with contents (if opening) and path of selected file
        -> m (Maybe a)

    -- | Get the name of the current rule
    getCurrentRuleName :: m (Maybe String)
    -- | Get the path of the current rule
    getCurrentRulePath :: m (Maybe FilePath)
    -- | Given a rule name, find its path and read its contents
    locateRuleByName
        :: String                      -- ^ rule name
        -> m (Maybe FilePath)          -- ^ another method to find a path, for if the rule cannot be found
        -> m (Maybe (FilePath, Text))  -- ^ the path and contents of the rule, if it was found
    -- | Get the text of the current rule
    getRuleText :: m Text
    -- | Get the language the current rule is written in
    getCurrentLang :: m T.Rule
    -- | Write text of rule to a file
    writeRule :: FilePath -> Text -> m ()
    -- | Set the rule window to display a specific rule
    setRuleWindowRule
        :: Text    -- ^ Text of rule
        -> T.Rule  -- ^ Language of rule
        -> m ()
    -- | Set the current rule
    setCurrentRule
        :: Maybe FilePath  -- ^ Path to rule, if there is one
        -> String          -- ^ Text of rule
        -> T.Rule          -- ^ Rule type
        -> m ()

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
    OpenFile :: FileChooserAction True
    SaveFile :: FileChooserAction False

-- | Convert a 'FileChooserAction' to the GTK equivalent
toGtkAction :: FileChooserAction i -> GI.Gtk.FileChooserAction
toGtkAction OpenFile = FileChooserActionOpen
toGtkAction SaveFile = FileChooserActionSave

-- | @Optional p i@ returns @p@ if @i@ is true, otherwise returns
-- @()@.
type family Optional p i where
    Optional p True = p
    Optional p False = ()

-- | Describes the difference between a previously recorded point
-- and another point. Used as the output of 'recordNewMousePoint'.
data PointDiff = NoDiff    -- ^ No difference
               | NewPoint  -- ^ There was no previously recorded point
               | PointDiff Point
               -- ^ There was a previously recorded point, with the given difference
               -- (i.e. if you have @PointDiff d@, then @prevPoint + d = newPoint@)
               deriving (Show)

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

-- * Concrete 'App' monad

-- | A concrete implementation of the 'MonadApp' interface.
newtype App a = App { getApp :: ReaderT T.Application IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader T.Application)

-- | Given a 'T.Application' state, run an 'App' in 'IO'.
runApp :: App a -> T.Application -> IO a
runApp = runReaderT . getApp

-- | Read an 'IORef' from 'T.Application' in 'App', using a 'Lens''.
readIORef' :: Lens' T.Application (IORef a) -> App a
readIORef' l = asks (^. l) >>= liftIO . readIORef

-- | Write to an 'IORef' from 'T.Application' in 'App', using a 'Lens''.
writeIORef' :: Lens' T.Application (IORef a) -> a -> App ()
writeIORef' l v = asks (^. l) >>= \r -> liftIO $ writeIORef r v

-- | Modify an 'IORef' from 'T.Application' in 'App', using a 'Lens''.
modifyIORefA' :: Lens' T.Application (IORef a) -> (a -> a) -> App ()
modifyIORefA' l f = asks (^. l) >>= \r -> liftIO $ modifyIORef' r f

-- | Convert an 'IO' computation using 'T.Application' into an 'App'.
withApp :: (T.Application -> IO a) -> App a
withApp = App . ReaderT

-- | Redraw the canvas. Used internally in the implementation of
-- @instance MonadApp app@.
redrawCanvas :: App ()
redrawCanvas = asks (^. T.canvas) >>= liftIO . postGUIASync . widgetQueueDraw

-- | Set the icon of the ‘play\/pause button’ to ‘play’. Used
-- internally in the implementation of @instance MonadApp app@.
setPlayBtnIcon :: App ()
setPlayBtnIcon = withApp $ \app -> imageSetFromStock (app ^. T.runIcon) (pack "gtk-media-play")  $ U.param IconSizeButton

-- | Set the icon of the ‘play\/pause button’ to ‘pause’. Used
-- internally in the implementation of @instance MonadApp app@.
setPauseBtnIcon :: App ()
setPauseBtnIcon = withApp $ \app -> imageSetFromStock (app ^. T.runIcon) (pack "gtk-media-pause") $ U.param IconSizeButton

instance MonadApp App where
    getOps = do
        sRef <- asks (^. T.existState)
        (T.ExistState s) <- liftIO $ readIORef sRef
        return Ops
            { getPattern = s ^. (T.currentPattern . _1)
            , getRule = s ^. T.rule
            , modifyPattern = \r -> do
                let (u, g) = s ^. T.currentPattern
                liftIO $ writeIORef sRef $ T.ExistState $ s & T.currentPattern .~ r u g
                redrawCanvas
            , getClipboard = s ^. T.clipboardContents
            , setClipboard = \u ->
                liftIO $ writeIORef sRef $ T.ExistState $ s & T.clipboardContents .~ u
            , defaultVal = s ^. T.defaultVal
            , defaultPattern = s ^. T.defaultPattern
            , states = s ^. T.states
            , encodeInt = s ^. T.encodeInt
            , decodeInt = s ^. T.decodeInt
            , state2color = s ^. T.state2color
            , setState2Color = \u ->
                liftIO $ writeIORef sRef $ T.ExistState $ s & T.state2color .~ u
            , getName = s ^. T.getName
            }

    getCurrentMode = readIORef' T.currentMode
    setMode m = do
        writeIORef' T.currentMode m
        asks (^. T.drawopts) >>= \w ->
            widgetSetSensitive w $ m == T.DrawMode
    getCurrentDrawingState = ask >>= \app -> liftIO $
        comboBoxGetActiveIter (app ^. T.curstate) >>= \case
            (False, _) -> return 0
            (True, iter) -> treeModelGetValue (app ^. T.curstatem) iter 0 >>= (fmap fromIntegral . fromGValue @Int32)

    togglePlayThread preOnAct onAct =
        readIORef' T.runThread >>= \case
            Just t -> do
                liftIO $ killThread t
                writeIORef' T.runThread Nothing
                setPlayBtnIcon
            Nothing -> do
                _ <- preOnAct
                app <- ask
                t <- liftIO $ forkIO $ forever $ flip runApp app $ do
                    _ <- onAct
                    readIORef' T.delay >>= liftIO . threadDelay
                writeIORef' T.runThread (Just t)
                setPauseBtnIcon
    forceKillThread =
        readIORef' T.runThread >>= \case
            Nothing -> return ()
            Just t -> do
                liftIO $ killThread t
                writeIORef' T.runThread (Just t)
                setPlayBtnIcon

    saveRestorePattern = withApp $ \app -> do
        p <- readIORef $ app ^. T.pos
        T.modifyState app $ \state ->
            let T.ExistState'{T._currentPattern=(g, _), T._saved=s} = state
            in state & T.saved .~ Just (fromMaybe (g, p) s)
    restorePattern = do
        app <- ask
        liftIO $ T.modifyStateM app $ \state ->
            case state ^. T.saved of
                Just prev -> do
                    writeIORef (app ^. T.pos) $ snd prev
                    return $ state & T.saved .~ Nothing
                                & (T.currentPattern . _1) .~ fst prev
                Nothing -> pure state
        redrawCanvas
    resetRestorePattern = withApp $ \app ->
        T.modifyState app $ T.saved .~ Nothing

    modifyGen f = withApp $ flip U.modifyGeneration f
    modifyDelay f = do
        old <- readIORef' T.delay
        let new = f old
        writeIORef' T.delay new
        asks (^. T.delayLbl) >>= \l -> liftIO $ labelSetText l (pack $ show new)
    setCoordsLabel l = asks (^. T.coordsLbl) >>= flip labelSetText l

    getSelection = readIORef' T.selection
    setSelection s = writeIORef' T.selection s >> redrawCanvas

    getPos = readIORef' T.pos
    modifyPos f = modifyIORefA' T.pos f >> redrawCanvas

    modifyCellPos fCell fX fY = do
        ask >>= \app -> liftIO $
            modifyIORef (app ^. T.pos) $ over T.cellWidth  fCell
                                       . over T.cellHeight fCell
                                       . over T.leftXCoord fX
                                       . over T.topYCoord  fY
        redrawCanvas

    recordNewMousePoint p = do
        lastPoint <- readIORef' T.lastPoint
        writeIORef' T.lastPoint $ Just p
        return $ case lastPoint of
            Nothing -> NewPoint
            Just lp ->
                if lp == p
                then NoDiff
                else PointDiff (getDiff lp p)
      where
        getDiff (Point lastX lastY) (Point thisX thisY) =
            Point (thisX-lastX) (thisY-lastY)

    getPasteSelectionOverlay = readIORef' T.pasteSelectionOverlay
    setPasteSelectionOverlay o = do
        writeIORef' T.pasteSelectionOverlay o
        redrawCanvas

    runGridSizeDialog (cols, rows) callback = do
        colsAdj <- asks (^. T.newNumColsAdjustment)
        rowsAdj <- asks (^. T.newNumRowsAdjustment)
        liftIO $ adjustmentSetValue colsAdj $ fromIntegral cols
        liftIO $ adjustmentSetValue rowsAdj $ fromIntegral rows   
        d <- asks (^. T.newGridSizeDialog)
        U.dialogRun' d >>= \case
            AnotherResponseType 1 -> do  -- OK button
                newCols <- floor <$> adjustmentGetValue colsAdj
                newRows <- floor <$> adjustmentGetValue rowsAdj
                callback (Coord newCols) (Coord newRows)
            _ -> pure ()
        widgetHide d
    showSettingsDialog = withApp S.showSettingsDialog

    showAboutDialog = withApp $ \app -> do
        a <- aboutDialogNew
        set a
            [ #transientFor := app ^. T.window
            , #programName  := "Cabasa"
            , #name         := "Cabasa"
            , #version      := "0.1"
            , #comments     := "An application for the simulation of arbitrary 2D cellular automata"
            , #authors      := ["Brad Neimann"]
            , #copyright    := "© Brad Neimann 2017-2018"
            ]
        _ <- dialogRun a
        widgetDestroy a

    showUserManual = liftIO $ do
        location <- getDataFileName "doc/UserManual.pdf"
#ifdef mingw32_HOST_OS
        callCommand $ "start " ++ location
#elif defined linux_HOST_OS
        callCommand $ "xdg-open" ++ location
#elif defined darwin_HOST_OS
        callCommand $ "open"     ++ location
#else
        showMessageDialog (Just window) MessageError ButtonsOk
            "ERROR: Could not figure out how to open manual on your system.\nThis is a bug - please report it on the project website" pure
#endif

    showErrorDialog msg = do
        w <- asks (^. T.window)
        liftIO $ U.showMessageDialog w MessageTypeError ButtonsTypeOk msg $ const $ pure ()
    showQueryDialog msg no yes = withApp $ \app -> do
        U.showMessageDialog (app ^. T.window) MessageTypeInfo ButtonsTypeYesNo msg
            $ flip runApp app . \case { ResponseTypeYes -> yes ; _ -> no }

    withPatternFileDialog act callback = withApp $ \app -> do
        U.withFileDialogChoice (U.getPatternFileChooser app) (toGtkAction act) $ \_ path ->
            case act of
                SaveFile -> runApp (callback () path) app
                OpenFile -> do
                    p <- TIO.readFile path
                    runApp (callback p path) app
    withRuleFileDialog act filterType callback = withApp $ \app -> do
        U.withFileDialogChoice (U.getRuleFileChooser app filterType) (toGtkAction act) $ \fChooser fName -> do
            -- There are two places where the rule type can be
            -- specified: the extension of the chosen file, or the
            -- chosen file type filter. We use the former if it is
            -- present, otherwise use the latter.
            let ruleTypeExt = case takeExtension fName of
                    ".alp" -> Just T.ALPACA
                    ".hs"  -> Just T.Hint
                    ".lhs" -> Just T.Hint
                    _      -> Nothing
            ruleTypeFilter <- fileChooserGetFilter fChooser >>= \case
                Just fFilter -> fileFilterGetName fFilter <&> \case
                    -- As we know that there are only two filters, the first
                    -- character of the filter offers a useful heuristic to
                    -- determine the file type
                    Just (uncons -> Just (x, _)) -> case x of
                        'A' -> Just T.ALPACA
                        'H' -> Just T.Hint
                        _   -> Nothing
                    _ -> Nothing
                Nothing -> return Nothing
            let ruleType = ruleTypeExt <|> ruleTypeFilter
            case act of
                SaveFile -> runApp (callback ruleType () fName) app
                OpenFile -> do
                    p <- TIO.readFile fName
                    runApp (callback ruleType p fName) app
    withCSSFileDialog act callback = withApp $ \app -> do
        U.withFileDialogChoice (getCSSFileChooser app) (toGtkAction act) $ \_ path -> do
            case act of
                SaveFile -> runApp (callback () path) app
                OpenFile -> do
                    p <- TIO.readFile path
                    runApp (callback p path) app
      where
        getCSSFileChooser :: T.Application -> GI.Gtk.FileChooserAction -> IO FileChooserNative
        getCSSFileChooser app ac = do
            fChooser <- fileChooserNativeNew
                Nothing
                (Just $ app ^. T.editSheetWindow)
                ac
                Nothing Nothing
        
            cssFilter <- fileFilterNew
            fileFilterSetName cssFilter $ Just "ALPACA Stylesheets files (*.css)"
            fileFilterAddPattern cssFilter "*.css"
            fileChooserAddFilter fChooser cssFilter
        
            return fChooser

    getCurrentRuleName = withApp T.getCurrentRuleName
    getCurrentRulePath = readIORef' T.currentRulePath
    locateRuleByName rule cantFind = withApp $ \app -> do
        predefDir <- getSetting' T.predefinedRulesDir app
        userDir   <- getSetting' T.userRulesDir       app

        rules <- listDirectories [userDir, predefDir]
        let rulePathMay = find ((rule==) . takeBaseName) rules
        rulePath <- case rulePathMay of
            Nothing -> runApp cantFind app >>= \case
                Nothing -> return Nothing
                p@(Just _) -> return p
            p@(Just _) -> return p
        case rulePath of
            Nothing -> return Nothing
            Just path -> do
                contents <- TIO.readFile path
                return $ Just (path, contents)
      where
        listDirectories :: [FilePath] -> IO [FilePath]
        listDirectories ds =
            filterM doesDirectoryExist ds
            >>= traverse listDirectoryWithPath
            >>= (pure . concat)
          where
            listDirectoryWithPath dir = (fmap . fmap) (dir </>) $ listDirectory dir

    getRuleText = asks (^. T.newRuleBuf) >>= \newRuleBuf -> liftIO $ do
        (start, end) <- textBufferGetBounds newRuleBuf
        textBufferGetText newRuleBuf start end True
    getCurrentLang = withApp $ \app -> do
        alpacaOn <- checkMenuItemGetActive (app ^. T.alpacaLang)
        haskellOn <- checkMenuItemGetActive (app ^. T.haskellLang)
        return $ if | alpacaOn  -> T.ALPACA
                    | haskellOn -> T.Hint
                    | otherwise -> error "Error in radio button at getCurrentLang!\nThis is a bug; please report it to the package maintainer."
    writeRule path rule = do
        liftIO $ TIO.writeFile path rule
        writeIORef' T.currentRulePath (Just path)
    setRuleWindowRule ruleText ruleType = withApp $ \app -> do
        setTextBufferText (app ^. T.newRuleBuf) ruleText
        case ruleType of
            T.ALPACA -> checkMenuItemSetActive (app ^. T.alpacaLang)  True
            T.Hint   -> checkMenuItemSetActive (app ^. T.haskellLang) True
    setCurrentRule file contents ruleType = withApp $ \app -> U.setCurrentRule app file contents ruleType

    getCurrentPatternPath = readIORef' T.currentPatternPath
    setCurrentPatternPath = writeIORef' T.currentPatternPath . Just
    writePattern path contents = do
        liftIO $ writeFile path contents
        writeIORef' T.currentPatternPath $ Just path
    
    getStylesheetText = asks (^. T.sheetBuf) >>= \sheetBuf -> liftIO $ do
        (start, end) <- textBufferGetBounds sheetBuf
        textBufferGetText sheetBuf start end True
    getCurrentStylesheetPath = readIORef' T.currentStylesheetPath
    writeSheet file contents = asks (^. T.currentStylesheetPath) >>= \path -> liftIO $ do
        TIO.writeFile (file -<.> "css") contents
        writeIORef path (Just file)
    setStylesheetWindowStylesheet ss = do
        sheetBuf <- asks (^. T.sheetBuf)
        liftIO $ setTextBufferText sheetBuf $ pack ss

    type RenderContext App = GI.Cairo.Context
    -- from https://github.com/haskell-gi/haskell-gi/blob/36e4c4fb0df9e80d3c9b2f5999b65128e20317fb/examples/advanced/Cairo.hs#L297
    renderWithContext ct r = do
        canvas <- asks (^. T.canvas)
        liftIO $ do
            size <- getCanvasSize canvas
            withManagedPtr ct $ \p -> runReaderT (runRender $ r size) (Cairo (castPtr p))
      where
        getCanvasSize canvas = do
            w <- fromIntegral <$> widgetGetAllocatedWidth canvas
            h <- fromIntegral <$> widgetGetAllocatedHeight canvas
            return (w, h)
