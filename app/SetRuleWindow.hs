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

import Data.Text (unpack)
import System.FilePath

import Control.Monad.App.Class

setRuleWindowDeleteHandler :: MonadApp m => m ()
setRuleWindowDeleteHandler = do
    getCurrentRuleName >>= \case
        Just _ -> pure ()
        Nothing -> showQueryDialog "Do you want to save your changes" (pure ()) saveRuleAs
    setRuleWindowDelete

setRuleBtnHandler :: MonadApp m => m ()
setRuleBtnHandler = do
    text <- getRuleText
    setCurrentRule Nothing (unpack text)

saveRule :: MonadApp m => m ()
saveRule = getCurrentRulePath >>= \case
    Nothing    -> saveRuleAs
    Just fName -> writeCurrentRule fName

saveRuleAs :: MonadApp m => m ()
saveRuleAs =
    void $ withRuleFileDialog SaveFile $ \() fName ->
        writeCurrentRule $ fName -<.> "alp"

writeCurrentRule :: MonadApp m => FilePath -> m ()
writeCurrentRule file = getRuleText >>= writeRule file

openRuleHandler :: MonadApp m => m ()
openRuleHandler = void $
    withRuleFileDialog OpenFile $ \ruleText _ ->
        setRuleWindowRule ruleText
