{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StylesheetWindow (addStylesheetWindowHandlers) where

import Prelude hiding (readFile, writeFile)

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef

import qualified CA.ALPACA.Stylesheets as SS
import Data.Text (pack, unpack)
import Data.Text.IO (readFile, writeFile)
import GI.Gtk
import Lens.Micro
import System.FilePath

import Utils
import qualified Types as T

addStylesheetWindowHandlers :: T.Application -> IO ()
addStylesheetWindowHandlers app = do
    _ <- on (app ^. T.editSheetWindow) #deleteEvent $ \_ -> liftIO $ stylesheetWindowDeleteHandler app

    _ <- on (app ^. T.editSheetWindowSetBtn) #clicked $ setBtnHandler app

    _ <- on (app ^. T.saveSheetAs) #activate $ saveSheetHandler app
    _ <- on (app ^. T.saveSheetAs) #activate $ saveSheetAsHandler app
    _ <- on (app ^. T.openSheet)   #activate $ openSheetHandler app
    return ()

stylesheetWindowDeleteHandler :: T.Application -> IO Bool
stylesheetWindowDeleteHandler app = True <$ widgetHide (app ^. T.editSheetWindow)

setBtnHandler :: T.Application -> IO ()
setBtnHandler app = do
    (start, end) <- textBufferGetBounds (app ^. T.sheetBuf)
    sty <- textBufferGetText (app ^. T.sheetBuf) start end True
    case SS.parseStylesheet (unpack sty) of
        Left err ->
            showMessageDialog (app ^. T.window)
                              MessageTypeError
                              ButtonsTypeOk
                              ("Parse error:\n" <> pack err)
                              (const $ return ())
        Right sty' -> T.modifyState app $ \(st :: T.ExistState' t) ->
            let state2color' :: t -> (Double, Double, Double)
                state2color' state =
                    let fallback = (st ^. T.state2color) state
                    in
                        case ((st ^. T.getName) state) of
                            Nothing -> fallback
                            Just name ->
                                case (lookup (SS.Class name) sty') of
                                    Nothing -> fallback
                                    Just rules -> case rules of
                                        (SS.Fill (SS.RGB r g b) : _)
                                            -> (r, g, b)
                                        []  -> fallback
            in st & T.state2color .~ state2color'
    widgetQueueDraw $ app ^. T.canvas

saveSheetHandler :: T.Application -> IO ()
saveSheetHandler app = readIORef (app ^. T.currentStylesheetPath) >>= \case
    Nothing    -> saveSheetAsHandler app
    Just fName -> writeCurrentSheet app fName

saveSheetAsHandler :: T.Application -> IO ()
saveSheetAsHandler app = void $
    withFileDialogChoice (getCSSFileChooser app) FileChooserActionSave $
        const $ writeCurrentSheet app

writeCurrentSheet :: T.Application -> FilePath -> IO ()
writeCurrentSheet app fName = do
    (start, end) <- textBufferGetBounds (app ^. T.sheetBuf)
    writeFile (fName -<.> "css") =<< textBufferGetText (app ^. T.sheetBuf) start end True
    writeIORef (app ^. T.currentStylesheetPath) (Just fName)

openSheetHandler :: T.Application -> IO ()
openSheetHandler app = void $
    withFileDialogChoice (getCSSFileChooser app) FileChooserActionOpen $ \_ fName ->
        setTextBufferText (app ^. T.sheetBuf) =<< readFile fName

getCSSFileChooser :: T.Application -> FileChooserAction -> IO FileChooserNative
getCSSFileChooser app ac = do
    fChooser <- fileChooserNativeNew
        Nothing
        (Just $ app ^. T.editSheetWindow)
        ac
        Nothing Nothing

    cssFilter <- fileFilterNew
    fileFilterSetName cssFilter $ Just "ALPACA Stylesheets files (*.css)"
    fileFilterAddPattern cssFilter "*.css"
    fileChooserAddFilter fChooser cssFilter

    return fChooser
