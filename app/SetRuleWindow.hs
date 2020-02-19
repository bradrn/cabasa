{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module SetRuleWindow
    ( setRuleWindowDeleteHandler
    , setRuleBtnHandler
    , saveRule
    , saveRuleAs
    , openRuleHandler
    ) where

import Prelude hiding (readFile, writeFile)

import Control.Monad (void)
import Data.Maybe (fromMaybe)

import Data.Text (unpack)
import System.FilePath

import Control.Monad.App.Class
import qualified Types as T

setRuleWindowDeleteHandler :: MonadApp m => m ()
setRuleWindowDeleteHandler = do
    getCurrentRuleName >>= \case
        Just _ -> pure ()
        Nothing -> showQueryDialog "Do you want to save your changes" (pure ()) saveRuleAs
    setRuleWindowDelete

setRuleBtnHandler :: MonadApp m => m ()
setRuleBtnHandler = do
    text <- getRuleText
    ruleType <- getCurrentLang
    setCurrentRule Nothing (unpack text) ruleType

saveRule :: MonadApp m => m ()
saveRule = getCurrentRulePath >>= \case
    Nothing    -> saveRuleAs
    Just fName -> writeCurrentRule fName

saveRuleAs :: MonadApp m => m ()
saveRuleAs = do
    curRuleType <- getCurrentLang
    void $ withRuleFileDialog SaveFile (Just curRuleType) $ \ruleType () fName ->
        writeCurrentRule $ case ruleType of
                Just T.ALPACA -> fName -<.> "alp"
                Just T.Hint   -> fName -<.> "hs"
                Nothing       -> fName

writeCurrentRule :: MonadApp m => FilePath -> m ()
writeCurrentRule file = getRuleText >>= writeRule file

openRuleHandler :: MonadApp m => m ()
openRuleHandler = void $
    withRuleFileDialog OpenFile Nothing $ \ruleType ruleText _ ->
        setRuleWindowRule ruleText $
            fromMaybe T.ALPACA ruleType  -- guess of rule type is unspecified
