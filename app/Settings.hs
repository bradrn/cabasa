{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Settings where

import Control.Monad (unless)
import Data.Functor (($>))
import Data.IORef
import Data.Maybe (fromJust, fromMaybe)

import Data.Text (pack)
import Data.Yaml
import GI.Gtk
import Lens.Micro
import System.Directory
import System.FilePath ((</>))

import ShowDialog
import qualified Types as T

configDir :: IO FilePath
configDir = getXdgDirectory XdgConfig "Cabasa"

settingsLocation :: IO FilePath
settingsLocation = (</>) <$> configDir <*> pure "settings.yaml"
    
defaultSettings :: IO T.Settings
defaultSettings = do
    _predefinedRulesDir <- Just <$> getXdgDirectory XdgConfig "Cabasa/Rules"
    _userRulesDir       <- Just <$> (getUserDocumentsDirectory <&> (</> "Cabasa"))
    return T.Settings{_gridSize = Just (100, 100), ..}

readSettings :: Window -> IO T.Settings
readSettings win = do
    l <- settingsLocation
    -- decodeFileEither does this check for us anyway, but doing it ourself
    -- gives better error messages
    doesFileExist l >>= \case
        False ->
            showMessageDialog
                win
                MessageTypeError
                ButtonsTypeYesNo
                ("Settings file does not exist.\n\nDo you want to create a settings file with default settings?")
            $ \case
                ResponseTypeYes
                    -> defaultSettings >>= \def -> writeSettings def $> def
                _   -> defaultSettings
        True ->
            decodeFileEither @T.Settings l >>= \case
                Left err ->
                    showMessageDialog
                        win
                        MessageTypeError
                        ButtonsTypeOk
                        ("Error when reading settings:\n"
                         <> pack (prettyPrintParseException err)
                         <> "\nUsing default settings.")
                        (const $ pure ())
                    >> defaultSettings
                Right ss -> pure ss

saveSettings :: T.Settings -> T.Application -> IO ()
saveSettings ss app = writeIORef (app ^. T.settings) ss >> writeSettings ss

writeSettings :: T.Settings -> IO ()
writeSettings ss = do
    f <- settingsLocation
    -- create file and/or its parents if it doesn't exist
    createDirectoryIfMissing True =<< configDir
    doesFileExist f >>= \x -> unless x $ writeFile f ""
    encodeFile f ss

{-
Reading settings
================

The basic setting-reading function is getSetting, which gets an `a`
from a `T.Application` using a `Traversal'`. (It uses a Traversal and
not a Lens because it's more composable - the settings all have type
`Maybe <something>`, and so by doing `settingLens . _Just` you can get
a `Traversal' T.Settings <something>` which can be composed with other
Lenses and Traversals).

As well as that basic function there are also three other functions:
- The `<function>From` functions get a setting from an IORef as
  opposed to a T.Application - useful if a setting is needed before a
  T.Application has been fully constructed
- The `<function>'` functions get a setting using a Lens instead of a
  Traversal
-}

getSetting :: Traversal' T.Settings a -> T.Application -> IO a
getSetting s app = getSettingFrom s (app ^. T.settings)

getSetting' :: Lens' T.Settings (Maybe a) -> T.Application -> IO a
getSetting' s app = getSetting (s . _Just) app

getSettingFrom :: Traversal' T.Settings a -> IORef T.Settings -> IO a
getSettingFrom s ss = do
    def <- defaultSettings
    -- fromJust is safe here because defaultSettings should define a value for
    -- every field
    let defField = fromJust $ def ^? s
    curField <- (^? s) <$> readIORef ss
    return $ fromMaybe defField curField

getSettingFrom' :: Lens' T.Settings (Maybe a) -> IORef T.Settings -> IO a
getSettingFrom' s ss = getSettingFrom (s . _Just) ss

changeSetting :: Lens' T.Settings a -> a -> T.Application -> IO ()
changeSetting s val app = modifyIORef' (app ^. T.settings) $ (s .~ val)
