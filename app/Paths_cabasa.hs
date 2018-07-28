module Paths_cabasa where

import System.FilePath (combine)

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . combine "./data"
