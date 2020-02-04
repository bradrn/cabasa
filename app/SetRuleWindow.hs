{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module SetRuleWindow (addSetRuleWindowHandlers) where

import Prelude hiding (readFile, writeFile)

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)

import Data.Text (unpack)
import GI.Gtk
import Lens.Micro
import System.FilePath

import Control.Monad.App
import qualified Types as T

addSetRuleWindowHandlers :: T.Application -> IO ()
addSetRuleWindowHandlers app = do
    _ <- on (app ^. T.setRuleWindow) #deleteEvent $ \_ -> liftIO $ setRuleWindowDeleteHandler app
    _ <- on (app ^. T.setRuleBtn)    #clicked  $ runApp setRuleBtnHandler app
    _ <- on (app ^. T.saveRule)      #activate $ runApp saveRule app
    _ <- on (app ^. T.saveRuleAs)    #activate $ runApp saveRuleAs app
    _ <- on (app ^. T.openRule)      #activate $ runApp openRuleHandler app
    return ()

setRuleWindowDeleteHandler :: T.Application -> IO Bool
setRuleWindowDeleteHandler app = do
    runApp ruleWindowDeleteActions app
    widgetHide (app ^. T.setRuleWindow)
    return True

ruleWindowDeleteActions :: MonadApp m => m ()
ruleWindowDeleteActions =
    getCurrentRuleName >>= \case
        Just _ -> pure ()
        Nothing -> showQueryDialog "Do you want to save your changes" (pure ()) saveRuleAs


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
