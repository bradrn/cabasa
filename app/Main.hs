{-# LANGUAGE CPP                         #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE MultiWayIf                  #-}
{-# LANGUAGE NamedFieldPuns              #-}
{-# LANGUAGE RankNTypes                  #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TupleSections               #-}
{-# LANGUAGE TypeApplications            #-}
{-# LANGUAGE ViewPatterns                #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main (main) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Monad ((=<<), forever, replicateM, void, when)
import Data.Foldable (for_, find)
import Data.IORef
import Data.Maybe (isJust, fromMaybe)
import Data.Proxy
import GHC.TypeLits (natVal)
import System.Process (callCommand)

import Control.Comonad.Store hiding (pos)
import qualified Data.Finite as F
import Data.Text (pack)
import Graphics.Rendering.Cairo hiding (clip)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.General.CssProvider
import Graphics.UI.Gtk.General.StyleContext
import Lens.Micro hiding (set)
import System.Directory (listDirectory, doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath (combine, takeBaseName, takeExtension, (-<.>))

import CA hiding (pos)
import CA.ALPACA
import qualified CA.Format.MCell as MC
import CA.Utils (conwayLife)
import Canvas
import Hint
import Hint.Interop
import Paths_cabasa
import qualified Types as T

main :: IO ()
main = do
    initGUI
    builder <- builderNew
    builderAddFromFile builder =<< getDataFileName "cabasa.glade"
    prov <- cssProviderNew
    cssProviderLoadFromPath prov =<< getDataFileName "cabasa.css"
    screenGetDefault >>= \case
        Just screen -> styleContextAddProviderForScreen screen prov 800
        Nothing -> return ()

    _window <- builderGetObject builder castToWindow      "window"

    ------- Menu widgets --------------

    _savePatternAs <- builderGetObject builder castToMenuItem "savePatternAs"
    _openPattern   <- builderGetObject builder castToMenuItem "openPattern"
    _quit          <- builderGetObject builder castToMenuItem    "quit"
    _setRule       <- builderGetObject builder castToMenuItem    "setRule"
    _clearPattern  <- builderGetObject builder castToMenuItem "clearPattern"
    _drawMode      <- builderGetObject builder castToMenuItem "drawMode"
    _moveMode      <- builderGetObject builder castToMenuItem "moveMode"
    _about         <- builderGetObject builder castToMenuItem    "about"
    _uman          <- builderGetObject builder castToMenuItem  "uman"

    ------- Main window widgets -------

    _run           <- builderGetObject builder castToButton      "run"
    _runIcon       <- builderGetObject builder castToImage       "runIcon"
    _step          <- builderGetObject builder castToButton      "step"
    _reset         <- builderGetObject builder castToButton      "reset"
    _canvas        <- builderGetObject builder castToDrawingArea "canvas"
    _generationLbl <- builderGetObject builder castToLabel "generation"
    _drawopts      <- builderGetObject builder castToBox       "drawopts"
    _curstate      <- builderGetObject builder castToComboBox  "curstate"

    _curstatem <- listStoreNew [0, 1]
    comboBoxSetModel _curstate (Just _curstatem)
    cellLayoutClear _curstate
    curstateRenderer <- cellRendererTextNew
    cellLayoutPackStart _curstate curstateRenderer True
    cellLayoutSetAttributes _curstate curstateRenderer _curstatem (pure . (cellText :=) . show)

    ------- Set new rule dialog -------

    _setRuleWindow <- builderGetObject builder castToWindow   "setRuleWindow"
    _setRuleBtn    <- builderGetObject builder castToButton   "setRuleBtn"
    _newRuleBuf    <- builderGetObject builder castToTextView "newRuleView" >>= textViewGetBuffer
    _alpacaLang    <- builderGetObject builder castToRadioMenuItem "alpacaLang"
    _haskellLang   <- builderGetObject builder castToRadioMenuItem "haskellLang"
    _saveRuleAs    <- builderGetObject builder castToMenuItem "saveRuleAs"
    _openRule      <- builderGetObject builder castToMenuItem "openRule"

    let guiObjects = T.GuiObjects{..}

    _existState <- do
        s <- getStdGen
        let _rule = pure . conwayLife
            _states = [False, True]
            _defaultPattern = fromList $ replicate 100 $ replicate 100 False
            _state2color st = if st then (0,0,0) else (1,1,1)
            _encodeInt = fromEnum
            _decodeInt 1 = True
            _decodeInt _ = False
            _currentPattern = (fromList $ replicate 100 $ replicate 100 False, s)
            _saved = Nothing
        newIORef $ T.ExistState (T.ExistState'{_ca=CAVals'{..}, ..})
    _currentRuleName <- newIORef @(Maybe String) Nothing
    _pos             <- newIORef $
        T.Pos { _leftXCoord = 0, _topYCoord = 0, _cellWidth = 16, _cellHeight = 16 }
    _runThread       <- newIORef @(Maybe ThreadId) Nothing
    _lastPoint       <- newIORef @(Maybe CA.Point) Nothing
    _generation      <- newIORef @Int 0
    _currentMode     <- newIORef T.DrawMode

    let ioRefs = T.IORefs{..}

    colors <- randomColors

    let app =
          T.Application { T._colors        = colors
                        , T._appGuiObjects = guiObjects
                        , T._appIORefs     = ioRefs
                        }

    addCanvasHandlers app modifyGeneration

    _drawMode `on` menuItemActivated $ writeIORef _currentMode T.DrawMode >> widgetSetSensitive _drawopts True
    _moveMode `on` menuItemActivated $ writeIORef _currentMode T.MoveMode >> widgetSetSensitive _drawopts False

    _step `on` buttonActivated $ savePattern app >> runGen app postGUIAsync
    _run `on` buttonActivated $ readIORef _runThread >>= \case
        Just t -> do
            killThread t
            writeIORef _runThread Nothing
            imageSetFromStock _runIcon (pack "gtk-media-play") IconSizeButton
        Nothing -> do
            savePattern app
            t <- forkIO $ forever $ runGen app postGUISync >> threadDelay 100000
            writeIORef _runThread $ Just t
            imageSetFromStock _runIcon (pack "gtk-media-pause") IconSizeButton
    _reset `on` buttonActivated $ do
        readIORef _runThread >>= \case
            Just t -> do
                killThread t
                writeIORef _runThread Nothing
                imageSetFromStock _runIcon (pack "gtk-media-play") IconSizeButton
            Nothing -> return ()
        popPattern app
        widgetQueueDraw _canvas

    _savePatternAs `on` menuItemActivated $ void $
        withFileDialogChoice (getPatternFileChooser app) FileChooserActionSave $ const $ \fName ->
        T.withState app $ \state -> do
            ruleName <- readIORef _currentRuleName
            let p = state ^. T.currentPattern . _1
                ss = state ^. T.states
                encode = state ^. T.encodeInt
                mc = MC.MCell { MC.game = Just MC.SpecialRules
                              , rule = ruleName
                              , MC.speed = Nothing
                              , MC.ccolors = Just (length ss)
                              , MC.coloring = Nothing
                              , MC.wrap = Just True
                              , MC.palette = Nothing
                              , MC.description = Nothing
                              , MC.universe = encode <$> p
                              , MC.diversities = []
                              }
            writeFile (fName -<.> "mcl") $ MC.encodeMCell mc
    _openPattern `on` menuItemActivated $ void $
        withFileDialogChoice (getPatternFileChooser app) FileChooserActionOpen $ const $ \fName -> do
            pat <- readFile fName
            case MC.decodeMCell pat of
                Left err -> showMessageDialog (Just $ app ^. T.window) MessageError ButtonsOk
                    ("Could not decode file! The error was:\n" ++ err)
                    (const $ pure ())
                -- We rename one field to avoid shadowing Hint.Interop.rule
                Right MC.MCell{rule=rule'mc, ..} -> do
                    case rule'mc of
                        Nothing -> return ()
                        Just rule' -> (maybe True (rule'==) <$> readIORef _currentRuleName) >>= flip when (do
                            showMessageDialog (Just $ app ^. T.window) MessageInfo ButtonsYesNo
                                "This pattern is set to use a different rule to the rule currently loaded\nDo you want to change the rule to that specified in the pattern?"
                                $ \case
                                ResponseYes ->
                                    let findNewRule = showMessageDialog (Just $ app ^. T.window) MessageWarning ButtonsYesNo
                                            ("Could not find the specified rule '" ++ rule' ++ "'.\nDo you want to find this rule manually?")
                                            $ \case
                                            ResponseYes -> withFileDialogChoice (getRuleFileChooser app Nothing) FileChooserActionOpen $ const pure
                                            _ -> return Nothing
                                    in do
                                        dataFile <- getDataFileName "Rules"
                                        listDirectory dataFile
                                          >>= pure . find ((rule'==) . takeBaseName)
                                          >>= (\case { Just a -> pure $ Just a; Nothing -> findNewRule}) >>= \case
                                            Nothing -> return ()
                                            Just (combine dataFile -> file) -> do
                                                text <- readFile $ file
                                                let ruleType = case (takeExtension file) of
                                                        ".hs"  -> T.Hint
                                                        ".alp" -> T.ALPACA
                                                        _ -> T.ALPACA -- guess
                                                setCurrentRule app (Just rule') text ruleType
                                                -- Set this rule's text in the Set Rule dialog
                                                textBufferSetText _newRuleBuf text
                                _ -> return ())

                    T.modifyState app $ \state ->
                        let fn = state ^. T.decodeInt
                        in state & (T.currentPattern . _1) .~ (fn <$> universe)
                    widgetQueueDraw _canvas

    _about `on` menuItemActivated $ do
        a <- aboutDialogNew
        set a
            [ windowTransientFor     := app ^. T.window
            , aboutDialogProgramName := "Cabasa"
            , aboutDialogName        := "Cabasa"
            , aboutDialogVersion     := "0.1"
            , aboutDialogComments    := "An application for the simulation of arbitrary 2D cellular automata"
            , aboutDialogAuthors     := ["Brad Neimann"]
            , aboutDialogCopyright   := "© Brad Neimann 2017-2018"
            ]
        dialogRun a
        widgetDestroy a
    _uman `on` menuItemActivated $ void $ do
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
    _quit `on` menuItemActivated $ mainQuit

    _setRule `on` menuItemActivated $ widgetShowAll _setRuleWindow
    _setRuleWindow `on` deleteEvent $ liftIO $ do
        readIORef _currentRuleName >>= \case
            Just _  -> pure ()
            Nothing ->
                showMessageDialog
                    (Just _setRuleWindow)
                    MessageQuestion
                    ButtonsYesNo
                    "Do you want to save your changes?" $ \case
                        ResponseYes -> menuItemEmitActivate _saveRuleAs
                        _ -> pure ()
        widgetHide _setRuleWindow
        return True
    _setRuleBtn `on` buttonActivated $ do
       (start, end) <- textBufferGetBounds _newRuleBuf
       text <- textBufferGetText @_ @String _newRuleBuf start end True
       ruleType <- getCurrentLang app
       setCurrentRule app Nothing text ruleType
    _newRuleBuf `on` bufferChanged $ writeIORef _currentRuleName Nothing
    _saveRuleAs `on` menuItemActivated $ void $ do
        ruleType <- getCurrentLang app
        withFileDialogChoice (getRuleFileChooser app $ Just ruleType) FileChooserActionSave $ \fChooser fName -> do
            (start, end) <- textBufferGetBounds _newRuleBuf
            text <- textBufferGetText @_ @String _newRuleBuf start end True
            fileChooserGetFilter fChooser >>= \case
                Just fFilter -> fileFilterGetName fFilter >>= \case
                    -- As we know that there are only two filters, the first
                    -- character of the filter offers a useful heuristic to
                    -- determine the file type
                    ('A':_) -> writeFile (fName -<.> "alp") text
                    ('H':_) -> writeFile (fName -<.> "hs" ) text
                    _       -> writeFile  fName             text
                Nothing     -> writeFile  fName             text
            writeIORef _currentRuleName (Just $ takeBaseName fName)
    _openRule `on` menuItemActivated $ void $
        withFileDialogChoice (getRuleFileChooser app Nothing) FileChooserActionOpen $ const $ \fName -> do
            ruleText <- readFile fName
            textBufferSetText _newRuleBuf ruleText
            case takeExtension fName of
                ".alp" -> checkMenuItemSetActive _alpacaLang  True
                ".hs"  -> checkMenuItemSetActive _haskellLang True
                ".lhs" -> checkMenuItemSetActive _haskellLang True
                _      -> return ()

    _window `on` objectDestroy $ mainQuit
    widgetShowAll _window
    mainGUI

modifyGeneration :: T.Application -> (Int -> Int) -> IO ()
modifyGeneration app f = do
    let generation    = app ^. T.generation
        generationLbl = app ^. T.generationLbl
    g <- readIORef generation
    let g' = f g
    writeIORef generation g'
    labelSetText generationLbl $ show g'

-- When runGen is called from the main thread, we want to use
-- postGUIAsync, but when it's called from any other thread, we want to
-- use postGUISync - so we provide an argument to select the function.
-- See http://gtk2hs-users.narkive.com/QvCQw4q3/use-of-postguisync-within-the-main-gtk-thread
runGen :: T.Application -> (IO () -> IO ()) -> IO ()
runGen app postFn = do
    T.modifyState app $ \state ->
        let r = state ^. T.rule
            (g, s) = state ^. T.currentPattern
        in state & T.currentPattern .~ runRand (evolve r g) s
    modifyGeneration app (+1)
    postFn (widgetQueueDraw $ app ^. T.canvas)

savePattern :: T.Application -> IO ()
savePattern app = do
    p <- readIORef $ app ^. T.pos
    T.modifyState app $ \state ->
        let T.ExistState'{_currentPattern=(g, _), _saved=s} = state
        in state & T.saved .~ Just (fromMaybe (g, p) s)

popPattern :: T.Application -> IO ()
popPattern app = do
    modifyGeneration app (const 0)
    T.modifyStateM app $ \state ->
        case state ^. T.saved of
            Just prev -> do
                writeIORef (app ^. T.pos) $ snd prev
                return $ state & T.saved .~ Nothing
                               & (T.currentPattern . _1) .~ fst prev
            Nothing -> pure state

getCurrentLang :: T.Application -> IO T.Rule
getCurrentLang app = do
    alpacaOn <- checkMenuItemGetActive (app ^. T.alpacaLang)
    haskellOn <- checkMenuItemGetActive (app ^. T.haskellLang)
    return $ if | alpacaOn  -> T.ALPACA
                | haskellOn -> T.Hint
                | otherwise -> error "Error in radio button at getCurrentLang!\nThis is a bug; please report it to the package maintainer."

setCurrentRule :: T.Application -> Maybe String -> String -> T.Rule -> IO ()
setCurrentRule app name text ruleType = do
    -- 'fn' is the rule-parsing function, which varies depending on 'ruleType'.
    -- If the parsing operation succeeds ('Right'), it returns a tuple; the
    -- first element is the 'CAVals' which has been parsed, and the second is a
    -- 'Bool' stating if the screen needs to be cleared (as it may need to be if
    -- e.g. an ALPACA initial configuration has been defined and loaded into
    -- '_defaultPattern').
    let fn :: String -> IO (Either String (CAVals, Bool))
        fn = case ruleType of
                 T.ALPACA ->
                     let mkGrid (AlpacaData{ rule = (rule :: StochRule Universe StdGen (F.Finite n))
                                           , initConfig }) =
                             let maxVal = natVal (Proxy @n)
                             in (,isJust initConfig) $ CAVals $ CAVals'
                                 { _defaultPattern = case initConfig of
                                       Just p  -> fromList p
                                       Nothing -> fromList $ replicate 100 $ replicate 100 $ 0
                                 , _state2color = \s -> (app ^. T.colors) !! fromInteger (F.getFinite s)
                                 , _encodeInt = fromInteger . F.getFinite
                                 , _decodeInt = F.finite . min (maxVal-1) . toInteger
                                 , _states = F.finites
                                 , _rule = rule
                                 }
                     in return . fmap mkGrid . runALPACA @StdGen
                 T.Hint   -> (fmap . fmap . fmap) (,False) runHint
    fn text >>= \case
         Left err -> showMessageDialog (Just $ app ^. T.window)
                                       MessageError
                                       ButtonsOk
                                       err
                                       (const $ pure ())
         Right (CAVals _ca, pressClear) -> do
             g <- newStdGen
             T.withState app $ \old -> do
                 let encFn = old ^. T.encodeInt
                     decFn = _ca ^. T.decodeInt
                     (oldPtn, _) = old ^. T.currentPattern
                     newPtn = (decFn . encFn) <$> oldPtn
                 writeIORef (app ^. T.existState) $ T.ExistState $
                     T.ExistState'{_ca, _currentPattern=(newPtn, g), _saved=Nothing}
             modifyGeneration app (const 0)
             writeIORef (app ^. T.currentRuleName) name

             -- Update the ListStore with the new states
             let curstatem = app ^. T.curstatem
                 curstate  = app ^. T.curstate
             (listStoreClear curstatem) >> forM_ (enumFromTo 0 $ length (_ca ^. T.states) - 1) (listStoreAppend curstatem)
             comboBoxSetActive curstate 0

             when pressClear $ menuItemEmitActivate (app ^. T.clearPattern)

             -- Because we're changing the currentPattern, we need to redraw
             widgetQueueDraw $ app ^. T.canvas

-- Returns a file chooser preconfigured to save or open rule files
getRuleFileChooser :: T.Application -> Maybe T.Rule -> FileChooserAction -> IO FileChooserDialog
getRuleFileChooser app filter action = do
    fChooser <- fileChooserDialogNew
        Nothing
        (Just $ app ^. T.setRuleWindow)
        action
        [("Cancel", ResponseCancel), ("OK", ResponseOk )]

    alpacaFilter <- fileFilterNew
    fileFilterSetName alpacaFilter "ALPACA files"
    fileFilterAddPattern alpacaFilter "*.alp"
    fileChooserAddFilter fChooser alpacaFilter

    haskellFilter <- fileFilterNew
    fileFilterSetName haskellFilter "Haskell files"
    fileFilterAddPattern haskellFilter "*.hs"
    fileFilterAddPattern haskellFilter "*.lhs"
    fileChooserAddFilter fChooser haskellFilter

    case filter of
        Nothing -> pure ()
        Just T.ALPACA -> fileChooserSetFilter fChooser alpacaFilter
        Just T.Hint   -> fileChooserSetFilter fChooser haskellFilter

    rulesDir <- getDataFileName "Rules/"
    rulesDirExists <- doesDirectoryExist rulesDir
    case action of
        FileChooserActionSave -> void $ do
            createDirectoryIfMissing True rulesDir
            fileChooserSetCurrentFolder fChooser rulesDir
        _ -> when rulesDirExists $ void $
            fileChooserSetCurrentFolder fChooser rulesDir

    return fChooser

-- Returns a file chooser preconfigured to save or open pattern files
getPatternFileChooser :: T.Application -> FileChooserAction -> IO FileChooserDialog
getPatternFileChooser app action = do
    fChooser <- fileChooserDialogNew
        Nothing
        (Just $ app ^. T.setRuleWindow)
        action
        [("Cancel", ResponseCancel), ("OK", ResponseOk )]

    mCellFilter <- fileFilterNew
    fileFilterSetName mCellFilter "MCell files (*.mcl)"
    fileFilterAddPattern mCellFilter "*.mcl"
    fileChooserAddFilter fChooser mCellFilter

    rulesDir <- getDataFileName "Patterns/"
    rulesDirExists <- doesDirectoryExist rulesDir
    case action of
        FileChooserActionSave -> void $ do
            createDirectoryIfMissing True rulesDir
            fileChooserSetCurrentFolder fChooser rulesDir
        _ -> when rulesDirExists $ void $
            fileChooserSetCurrentFolder fChooser rulesDir

    return fChooser

randomColors :: IO [(Double, Double, Double)]
randomColors = fmap (([(1, 1, 1), (0, 0, 0)]++) . tuplize) $ replicateM 3 $ randomRs @Double (0, 1) <$> newStdGen
  where
    tuplize [(a:as), (b:bs), (c:cs)] = (a, b, c):tuplize [as, bs, cs]
    tuplize _ = error "ERROR in tuplize"

withFileDialogChoice :: (FileChooserAction -> IO FileChooserDialog)
                     -> FileChooserAction
                     -> (FileChooserDialog -> FilePath -> IO a)
                     -> IO (Maybe a)
withFileDialogChoice constr action contn = do
    fChooser <- constr action
    result <- dialogRun fChooser >>= \case
        ResponseOk -> fileChooserGetFilename fChooser >>= \case
            Just fName -> Just <$> contn fChooser fName
            Nothing -> pure Nothing
        _ -> pure Nothing
    widgetDestroy fChooser
    return result

showMessageDialog :: Maybe Window -> MessageType -> ButtonsType -> String -> (ResponseId -> IO a) -> IO a
showMessageDialog window level buttons message fn = do
    d <- messageDialogNew window [DialogModal] level buttons message
    result <- dialogRun d
    widgetDestroy d
    fn result
