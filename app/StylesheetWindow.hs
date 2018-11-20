{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StylesheetWindow (addStylesheetWindowHandlers) where

import Data.IORef

import CA
import qualified CA.ALPACA.Stylesheets as SS
import Graphics.UI.Gtk
import Lens.Micro
import System.FilePath

import Utils
import qualified Types as T

addStylesheetWindowHandlers :: T.Application -> IO ()
addStylesheetWindowHandlers app = do
    _ <- (app ^. T.editSheetWindow) `on` deleteEvent $ liftIO $ stylesheetWindowDeleteHandler app

    _ <- (app ^. T.editSheetWindowSetBtn) `on` buttonActivated $ setBtnHandler app

    _ <- (app ^. T.saveSheetAs) `on` menuItemActivated $ saveSheetHandler app
    _ <- (app ^. T.saveSheetAs) `on` menuItemActivated $ saveSheetAsHandler app
    _ <- (app ^. T.openSheet) `on` menuItemActivated $ openSheetHandler app
    return ()

stylesheetWindowDeleteHandler :: T.Application -> IO Bool
stylesheetWindowDeleteHandler app = True <$ widgetHide (app ^. T.editSheetWindow)

setBtnHandler :: T.Application -> IO ()
setBtnHandler app = do
    (start, end) <- textBufferGetBounds (app ^. T.sheetBuf)
    sty <- textBufferGetText (app ^. T.sheetBuf) start end True
    case SS.parseStylesheet sty of
        Left err ->
            showMessageDialog (Just $ app ^. T.window)
                              MessageError
                              ButtonsOk
                              ("Parse error:\n" ++ err)
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
        textBufferSetText (app ^. T.sheetBuf) =<< readFile fName

getCSSFileChooser :: T.Application -> FileChooserAction -> IO FileChooserDialog
getCSSFileChooser app ac = do
    fChooser <- fileChooserDialogNew
        Nothing
        (Just $ app ^. T.editSheetWindow)
        ac
        [("Cancel", ResponseCancel), ("OK", ResponseOk)]

    cssFilter <- fileFilterNew
    fileFilterSetName cssFilter "ALPACA Stylesheets files (*.css)"
    fileFilterAddPattern cssFilter "*.css"
    fileChooserAddFilter fChooser cssFilter

    return fChooser
