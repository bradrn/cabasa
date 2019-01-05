{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE TypeApplications #-}

module SetRuleWindow (addSetRuleWindowHandlers) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef

import Graphics.UI.Gtk
import Lens.Micro
import System.FilePath

import Utils
import qualified Types as T

addSetRuleWindowHandlers :: T.Application -> IO ()
addSetRuleWindowHandlers app = do
    _ <- (app ^. T.setRuleWindow) `on` deleteEvent $ liftIO $ setRuleWindowDeleteHandler app
    _ <- (app ^. T.setRuleBtn) `on` buttonActivated $ setRuleBtnHandler app
    _ <- (app ^. T.saveRule)   `on` menuItemActivated $ saveRule app
    _ <- (app ^. T.saveRuleAs) `on` menuItemActivated $ saveRuleAs app
    _ <- (app ^. T.openRule) `on` menuItemActivated $ openRuleHandler app
    return ()

setRuleWindowDeleteHandler :: T.Application -> IO Bool
setRuleWindowDeleteHandler app = do
    (app & T.getCurrentRuleName) >>= \case
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

saveRule :: T.Application -> IO ()
saveRule app =
    readIORef (app ^. T.currentRulePath) >>= \case
        Nothing    -> saveRuleAs app
        Just fName -> writeCurrentRule app fName

saveRuleAs :: T.Application -> IO ()
saveRuleAs app = do
    ruleType <- getCurrentLang app
    void $
        withFileDialogChoice (getRuleFileChooser app $ Just ruleType) FileChooserActionSave $ \fChooser fName -> do
            fName' <- fileChooserGetFilter fChooser >>= \case
                Just fFilter -> fileFilterGetName fFilter <&> \case
                    -- As we know that there are only two filters, the first
                    -- character of the filter offers a useful heuristic to
                    -- determine the file type
                    ('A':_) -> fName -<.> "alp"
                    ('H':_) -> fName -<.> "hs"
                    _       -> fName
                Nothing     -> return fName
            writeCurrentRule app fName'

writeCurrentRule :: T.Application -> FilePath -> IO ()
writeCurrentRule app fName = do
    (start, end) <- textBufferGetBounds (app ^. T.newRuleBuf)
    text <- textBufferGetText @_ @String (app ^. T.newRuleBuf) start end True

    writeFile fName text
    writeIORef (app ^. T.currentRulePath) (Just fName)

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
