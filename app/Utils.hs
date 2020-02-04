{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Utils (module Utils, module ShowDialog) where

import Control.Monad (forM_, when)
import Data.Int (Int32)
import Data.IORef
import Data.Maybe (isJust)
import Data.Proxy
import Foreign.C.Types (CInt)
import GHC.TypeLits (natVal)

import CA.Universe
import CA.ALPACA
import Control.Monad.Random.Strict (newStdGen, Rand, StdGen)
import qualified Data.Finite as F
import Data.Text (pack)
import GI.Gtk
import Hint
import Lens.Micro
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)

import Hint.Interop
import Paths_cabasa
import Settings (getSetting')
import ShowDialog
import qualified Types as T

modifyGeneration :: T.Application -> (Int -> Int) -> IO ()
modifyGeneration app f = do
    let generation    = app ^. T.generation
        generationLbl = app ^. T.generationLbl
    g <- readIORef generation
    let g' = f g
    writeIORef generation g'
    labelSetText generationLbl $ pack $ show g'

setCurrentRule :: T.Application -> Maybe FilePath -> String -> T.Rule -> IO ()
setCurrentRule app path text ruleType =
    parseRule text >>= \case
         Left err -> showMessageDialog (app ^. T.window)
                                       MessageTypeError
                                       ButtonsTypeOk
                                       (pack err)
                                       (const $ pure ())
         Right (CAVals _ca, pressClear) -> do
             g <- newStdGen
             T.withState app $ \old -> do
                 let encFn = old ^. T.encodeInt
                     decFn = _ca ^. T.decodeInt
                     (oldPtn, _) = old ^. T.currentPattern
                     newPtn = (decFn . encFn) <$> oldPtn
                 writeIORef (app ^. T.existState) $ T.ExistState $
                     T.ExistState'{_ca, _currentPattern=(newPtn, g), _saved=Nothing, _clipboardContents=Nothing}
             modifyGeneration app (const 0)
             writeIORef (app ^. T.currentRulePath) path

             -- Update the ListStore with the new states
             let curstatem = app ^. T.curstatem
                 curstate  = app ^. T.curstate
             (listStoreClear curstatem)
             forM_ (enumFromTo 0 $ length (_ca ^. T.states) - 1) $ \val -> do
                 iter <- listStoreAppend curstatem
                 val' <- toGValue (fromIntegral val :: CInt)
                 listStoreSet curstatem iter [0] [val']
             comboBoxSetActive curstate 0

             when pressClear $ menuItemActivate (app ^. T.clearPattern)

             -- Because we're changing the currentPattern, we need to redraw
             widgetQueueDraw $ app ^. T.canvas
  where
    -- 'parseRule' is the rule-parsing function, which varies depending on 'ruleType'.
    -- If the parsing operation succeeds ('Right'), it returns a tuple; the
    -- first element is the 'CAVals' which has been parsed, and the second is a
    -- 'Bool' stating if the screen needs to be cleared (as it may need to be if
    -- e.g. an ALPACA initial configuration has been defined and loaded into
    -- '_defaultPattern').
    parseRule :: String -> IO (Either String (CAVals, Bool))
    parseRule = case ruleType of
             T.ALPACA -> \rule -> do
                     gridSize <- getSetting' T.gridSize app
                     return $ fmap (mkALPACAGrid gridSize) $ runALPACA @StdGen rule
             T.Hint   -> (fmap . fmap . fmap) (,False) runHint
      where
        mkALPACAGrid (numcols, numrows)
                     (AlpacaData{ rule = (rule :: CARuleA (Rand StdGen) Point (F.Finite n))
                                , initConfig
                                , stateData }) =
            let maxVal = natVal (Proxy @n)
            in (,isJust initConfig) $ CAVals $ CAVals'
                { _defaultSize = (Coord numcols, Coord numrows)
                , _defaultVal  = const 0
                , _state2color = \s -> (app ^. T.colors) !! fromInteger (F.getFinite s)
                , _encodeInt = fromInteger . F.getFinite
                , _decodeInt = F.finite . min (maxVal-1) . toInteger
                , _states = F.finites
                , _rule = rule
                , _getName = Just . fst . stateData}

-- Returns a file chooser preconfigured to save or open rule files
getRuleFileChooser :: T.Application -> Maybe T.Rule -> FileChooserAction -> IO FileChooserNative
getRuleFileChooser app filterType action = do
    fChooser <- fileChooserNativeNew
        Nothing
        (Just $ app ^. T.setRuleWindow)
        action
        Nothing Nothing

    alpacaFilter <- fileFilterNew
    fileFilterSetName alpacaFilter $ Just "ALPACA files"
    fileFilterAddPattern alpacaFilter "*.alp"
    fileChooserAddFilter fChooser alpacaFilter

    haskellFilter <- fileFilterNew
    fileFilterSetName haskellFilter $ Just "Haskell files"
    fileFilterAddPattern haskellFilter "*.hs"
    fileFilterAddPattern haskellFilter "*.lhs"
    fileChooserAddFilter fChooser haskellFilter

    case filterType of
        Nothing -> pure ()
        Just T.ALPACA -> fileChooserSetFilter fChooser alpacaFilter
        Just T.Hint   -> fileChooserSetFilter fChooser haskellFilter

    return fChooser

-- Returns a file chooser preconfigured to save or open pattern files
getPatternFileChooser :: T.Application -> FileChooserAction -> IO FileChooserNative
getPatternFileChooser app action = do
    fChooser <- fileChooserNativeNew
        Nothing
        (Just $ app ^. T.setRuleWindow)
        action
        Nothing Nothing

    mCellFilter <- fileFilterNew
    fileFilterSetName mCellFilter $ Just "MCell files (*.mcl)"
    fileFilterAddPattern mCellFilter "*.mcl"
    fileChooserAddFilter fChooser mCellFilter

    return fChooser

withFileDialogChoice :: (FileChooserAction -> IO FileChooserNative)
                     -> FileChooserAction
                     -> (FileChooserNative -> FilePath -> IO a)
                     -> IO (Maybe a)
withFileDialogChoice constr action contn = do
    fChooser <- constr action
    fmap (toEnum.fromIntegral) (#run fChooser) >>= \x ->
        if (x == ResponseTypeOk) || (x == ResponseTypeAccept) then
            fileChooserGetFilename fChooser >>= \case
                Just fName -> Just <$> contn fChooser fName
                Nothing -> pure Nothing
        else pure Nothing

-- Utilities for working with gi-gtk enums
-- Usage: like 'funcTakingAnEnum (param val)' or 'enum (funcReturningAnEnum)'

param :: Enum e => e -> Int32
param = fromIntegral . fromEnum

enum :: Enum e => Int32 -> e
enum = toEnum . fromIntegral
