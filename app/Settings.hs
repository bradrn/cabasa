{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Settings
       ( defaultSettings
       , settingsLocation
       , SettingsError(..)
       , readSettingsFile
       , writeSettings
       , Settings(..)
       , getSetting'
       , getSettingFrom
       , getSettingFrom'
       ) where

import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.IORef
import Data.Maybe (fromJust, fromMaybe)

import Data.Yaml
import Lens.Micro
import System.Directory
import System.FilePath ((</>))

import qualified Types as T
import qualified Types.Application as T

configDir :: IO FilePath
configDir = getXdgDirectory XdgConfig "Cabasa"

settingsLocation :: IO FilePath
settingsLocation = (</>) <$> configDir <*> pure "settings.yaml"
    
defaultSettings :: IO T.Settings
defaultSettings = do
    _predefinedRulesDir <- Just <$> getXdgDirectory XdgConfig "Cabasa/Rules"
    _userRulesDir       <- Just <$> (getUserDocumentsDirectory <&> (</> "Cabasa"))
    return T.Settings{_gridSize = Just (100, 100), ..}

data SettingsError = NonexistentFile | ParseError String

readSettingsFile :: FilePath -> IO (Either SettingsError T.Settings)
readSettingsFile f = doesFileExist f >>= \case
    False -> return $ Left NonexistentFile
    True -> first (ParseError . prettyPrintParseException) <$> decodeFileEither f

writeSettings :: T.Settings -> FilePath -> IO ()
writeSettings ss f = do
    -- create file and/or its parents if it doesn't exist
    createDirectoryIfMissing True =<< configDir
    doesFileExist f >>= \x -> unless x $ writeFile f ""
    encodeFile f ss

class Monad m => Settings m where
    saveSettings :: T.Settings -> m ()
    getSetting :: Traversal' T.Settings a -> m a

getSetting' :: Settings m => Lens' T.Settings (Maybe a) -> m a
getSetting' s = getSetting (s . _Just)

getSettingFrom :: Traversal' T.Settings a -> IORef T.Settings -> IO a
getSettingFrom s ss = do
    def <- defaultSettings
    -- fromJust is safe here because defaultSettings should define a value for
    -- every field
    let defField = fromJust $ def ^? s
    curField <- (^? s) <$> readIORef ss
    return $ fromMaybe defField curField

getSettingFrom' :: Lens' T.Settings (Maybe a) -> IORef T.Settings -> IO a
getSettingFrom' s = getSettingFrom (s . _Just)

changeSetting :: Lens' T.Settings a -> a -> T.Application -> IO ()
changeSetting s val app = modifyIORef' (app ^. T.settings) (s .~ val)
