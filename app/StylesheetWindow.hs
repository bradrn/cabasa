{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StylesheetWindow (addStylesheetWindowHandlers) where

import Prelude hiding (readFile, writeFile)

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)

import qualified CA.ALPACA.Stylesheets as SS
import Data.Text (pack, unpack)
import GI.Gtk
import Lens.Micro

import Control.Monad.App
import qualified Types as T

addStylesheetWindowHandlers :: T.Application -> IO ()
addStylesheetWindowHandlers app = do
    _ <- on (app ^. T.editSheetWindow) #deleteEvent $ \_ -> liftIO $ stylesheetWindowDeleteHandler app

    _ <- on (app ^. T.editSheetWindowSetBtn) #clicked $ runApp setBtnHandler app

    _ <- on (app ^. T.saveSheetAs) #activate $ runApp saveSheetHandler app
    _ <- on (app ^. T.saveSheetAs) #activate $ runApp saveSheetAsHandler app
    _ <- on (app ^. T.openSheet)   #activate $ runApp openSheetHandler app
    return ()

stylesheetWindowDeleteHandler :: T.Application -> IO Bool
stylesheetWindowDeleteHandler app = True <$ widgetHide (app ^. T.editSheetWindow)

setBtnHandler :: MonadApp m => m ()
setBtnHandler = do
    sty <- getStylesheetText
    case SS.parseStylesheet (unpack sty) of
        Left err -> showErrorDialog $ "Parse error:\n" <> pack err
        Right sty' -> getOps >>= \Ops{..} ->
            setState2Color $ \state ->
                fromMaybe (state2color state) $ do
                    name <- getName state
                    rules <- lookup (SS.Class name) sty'
                    case rules of
                        (SS.Fill (SS.RGB r g b) : _) -> return (r,g,b)
                        [] -> Nothing

saveSheetHandler :: MonadApp m => m ()
saveSheetHandler = getCurrentStylesheetPath >>= \case
    Nothing -> saveSheetAsHandler
    Just fName -> writeCurrentSheet fName

saveSheetAsHandler :: MonadApp m => m ()
saveSheetAsHandler = void $ withCSSFileDialog SaveFile $ const writeCurrentSheet

writeCurrentSheet :: MonadApp m => FilePath -> m ()
writeCurrentSheet file = getStylesheetText >>= writeSheet file

openSheetHandler :: MonadApp m => m ()
openSheetHandler = void $ withCSSFileDialog OpenFile $ \css _ ->
    setStylesheetWindowStylesheet $ unpack css
