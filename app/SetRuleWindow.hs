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
    _ <- (app ^. setRuleWindow) `on` deleteEvent $ liftIO $ do
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
    _ <- (app ^. setRuleBtn) `on` buttonActivated $ do
       (start, end) <- textBufferGetBounds (app ^. newRuleBuf)
       text <- textBufferGetText @_ @String (app ^. newRuleBuf) start end True
       ruleType <- getCurrentLang app
       setCurrentRule app Nothing text ruleType
    _ <- (app ^. newRuleBuf) `on` bufferChanged $ writeIORef (app ^. currentRuleName) Nothing
    _ <- (app ^. saveRuleAs) `on` menuItemActivated $ void $ do
        ruleType <- getCurrentLang app
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
    _ <- (app ^. openRule) `on` menuItemActivated $ void $
        withFileDialogChoice (getRuleFileChooser app Nothing) FileChooserActionOpen $ const $ \fName -> do
            ruleText <- readFile fName
            textBufferSetText (app ^. newRuleBuf) ruleText
            case takeExtension fName of
                ".alp" -> checkMenuItemSetActive (app ^. alpacaLang)  True
                ".hs"  -> checkMenuItemSetActive (app ^. haskellLang) True
                ".lhs" -> checkMenuItemSetActive (app ^. haskellLang) True
                _      -> return ()
    return ()
