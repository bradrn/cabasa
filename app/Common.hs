{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Common where

import Data.IORef
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import GHC.TypeLits (natVal)

import CA
import CA.ALPACA
import qualified Data.Finite as F
import Graphics.UI.Gtk
import Hint
import Lens.Micro
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)

import Hint.Interop
import Paths_cabasa
import qualified Types as T

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
