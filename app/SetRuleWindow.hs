{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module SetRuleWindow where

import Data.IORef

import CA
import Graphics.UI.Gtk
import Lens.Micro
import System.FilePath

import Common
import Types

addSetRuleWindowHandlers :: Application -> IO ()
addSetRuleWindowHandlers app = do
    _ <- (app ^. setRuleWindow) `on` deleteEvent $ liftIO $ setRuleWindowDeleteHandler app
    _ <- (app ^. setRuleBtn) `on` buttonActivated $ setRuleBtnHandler app
    _ <- (app ^. newRuleBuf) `on` bufferChanged $ writeIORef (app ^. currentRuleName) Nothing
    _ <- (app ^. saveRuleAs) `on` menuItemActivated $ saveRuleAsHandler app
    _ <- (app ^. openRule) `on` menuItemActivated $ openRuleHandler app
    return ()

setRuleWindowDeleteHandler :: Application -> IO Bool
setRuleWindowDeleteHandler app = do
    readIORef (app ^. currentRuleName) >>= \case
        Just _  -> pure ()
        Nothing ->
            showMessageDialog
                (Just (app ^. setRuleWindow))
                MessageQuestion
                ButtonsYesNo
                "Do you want to save your changes?" $ \case
                    ResponseYes -> menuItemEmitActivate (app ^. saveRuleAs)
                    _ -> pure ()
    widgetHide (app ^. setRuleWindow)
    return True

setRuleBtnHandler :: Application -> IO ()
setRuleBtnHandler app = do
    (start, end) <- textBufferGetBounds (app ^. newRuleBuf)
    text <- textBufferGetText @_ @String (app ^. newRuleBuf) start end True
    ruleType <- getCurrentLang app
    setCurrentRule app Nothing text ruleType

saveRuleAsHandler :: Application -> IO ()
saveRuleAsHandler app = do
    ruleType <- getCurrentLang app
    void $
        withFileDialogChoice (getRuleFileChooser app $ Just ruleType) FileChooserActionSave $ \fChooser fName -> do
            (start, end) <- textBufferGetBounds (app ^. newRuleBuf)
            text <- textBufferGetText @_ @String (app ^. newRuleBuf) start end True
            fileChooserGetFilter fChooser >>= \case
                Just fFilter -> fileFilterGetName fFilter >>= \case
                    -- As we know that there are only two filters, the first
                    -- character of the filter offers a useful heuristic to
                    -- determine the file type
                    ('A':_) -> writeFile (fName -<.> "alp") text
                    ('H':_) -> writeFile (fName -<.> "hs" ) text
                    _       -> writeFile  fName             text
                Nothing     -> writeFile  fName             text
            writeIORef (app ^. currentRuleName) (Just $ takeBaseName fName)

openRuleHandler :: Application -> IO ()
openRuleHandler app =
    void $
        withFileDialogChoice (getRuleFileChooser app Nothing) FileChooserActionOpen $ const $ \fName -> do
            ruleText <- readFile fName
            textBufferSetText (app ^. newRuleBuf) ruleText
            case takeExtension fName of
                ".alp" -> checkMenuItemSetActive (app ^. alpacaLang)  True
                ".hs"  -> checkMenuItemSetActive (app ^. haskellLang) True
                ".lhs" -> checkMenuItemSetActive (app ^. haskellLang) True
                _      -> return ()
