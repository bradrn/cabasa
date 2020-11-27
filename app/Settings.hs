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
    return T.Settings{_gridSize = Just (100, 100)}

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

changeSetting :: Lens' T.Settings a -> a -> T.Application n -> IO ()
changeSetting s val app = modifyIORef' (app ^. T.settings) (s .~ val)
