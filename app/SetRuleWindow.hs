{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE TypeApplications #-}

module SetRuleWindow (addSetRuleWindowHandlers) where

import Data.IORef

import CA
import Graphics.UI.Gtk
import Lens.Micro
import System.FilePath

import Utils
import qualified Types as T

addSetRuleWindowHandlers :: T.Application -> IO ()
addSetRuleWindowHandlers app = do
    _ <- (app ^. T.setRuleWindow) `on` deleteEvent $ liftIO $ setRuleWindowDeleteHandler app
    _ <- (app ^. T.setRuleBtn) `on` buttonActivated $ setRuleBtnHandler app
    _ <- (app ^. T.newRuleBuf) `on` bufferChanged $ writeIORef (app ^. T.currentRuleName) Nothing
    _ <- (app ^. T.saveRuleAs) `on` menuItemActivated $ saveRuleAsHandler app
    _ <- (app ^. T.openRule) `on` menuItemActivated $ openRuleHandler app
    return ()

setRuleWindowDeleteHandler :: T.Application -> IO Bool
setRuleWindowDeleteHandler app = do
    readIORef (app ^. T.currentRuleName) >>= \case
        Just _  -> pure ()
        Nothing ->
            showMessageDialog
                (Just (app ^. T.setRuleWindow))
                MessageQuestion
                ButtonsYesNo
                "Do you want to save your changes?" $ \case
                    ResponseYes -> menuItemEmitActivate (app ^. T.saveRuleAs)
                    _ -> pure ()
    widgetHide (app ^. T.setRuleWindow)
    return True

setRuleBtnHandler :: T.Application -> IO ()
setRuleBtnHandler app = do
    (start, end) <- textBufferGetBounds (app ^. T.newRuleBuf)
    text <- textBufferGetText @_ @String (app ^. T.newRuleBuf) start end True
    ruleType <- getCurrentLang app
    setCurrentRule app Nothing text ruleType

saveRuleAsHandler :: T.Application -> IO ()
saveRuleAsHandler app = do
    ruleType <- getCurrentLang app
    void $
        withFileDialogChoice (getRuleFileChooser app $ Just ruleType) FileChooserActionSave $ \fChooser fName -> do
            (start, end) <- textBufferGetBounds (app ^. T.newRuleBuf)
            text <- textBufferGetText @_ @String (app ^. T.newRuleBuf) start end True
            fileChooserGetFilter fChooser >>= \case
                Just fFilter -> fileFilterGetName fFilter >>= \case
                    -- As we know that there are only two filters, the first
                    -- character of the filter offers a useful heuristic to
                    -- determine the file type
                    ('A':_) -> writeFile (fName -<.> "alp") text
                    ('H':_) -> writeFile (fName -<.> "hs" ) text
                    _       -> writeFile  fName             text
                Nothing     -> writeFile  fName             text
            writeIORef (app ^. T.currentRuleName) (Just $ takeBaseName fName)

openRuleHandler :: T.Application -> IO ()
openRuleHandler app =
    void $
        withFileDialogChoice (getRuleFileChooser app Nothing) FileChooserActionOpen $ const $ \fName -> do
            ruleText <- readFile fName
            textBufferSetText (app ^. T.newRuleBuf) ruleText
            case takeExtension fName of
                ".alp" -> checkMenuItemSetActive (app ^. T.alpacaLang)  True
                ".hs"  -> checkMenuItemSetActive (app ^. T.haskellLang) True
                ".lhs" -> checkMenuItemSetActive (app ^. T.haskellLang) True
                _      -> return ()

getCurrentLang :: T.Application -> IO T.Rule
getCurrentLang app = do
    alpacaOn <- checkMenuItemGetActive (app ^. T.alpacaLang)
    haskellOn <- checkMenuItemGetActive (app ^. T.haskellLang)
    return $ if | alpacaOn  -> T.ALPACA
                | haskellOn -> T.Hint
                | otherwise -> error "Error in radio button at getCurrentLang!\nThis is a bug; please report it to the package maintainer."
