module Hint (runHint) where

import Data.Bifunctor (first)
import Data.List (intercalate, takeWhile)
import Data.List.Split (splitOn)

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe (unsafeSetGhcOption)
import System.IO.Temp (writeSystemTempFile)

import Hint.Interop

runHint :: String -> IO (Either String CAVals)
runHint ca =
    case splitOn "module " ca of
        (_:end:_) -> do
            let moduleName = takeWhile (/=' ') end
            path <- writeSystemTempFile (moduleName ++ ".hs") ca
            first showError <$> runInterpreter (mkInterpreter path moduleName)
        _          -> return $ Left $ "Error: File does not contain a valid Haskell module declaration!"
  where
    mkInterpreter path moduleName = do
        unsafeSetGhcOption "-Wall"
        unsafeSetGhcOption "-Werror"
        loadModules [path]
        setTopLevelModules [moduleName]
        setImports ["Prelude", "CA", "Data.Functor.Identity"]
        interpret "myCA" (as :: CAVals)

showError :: InterpreterError -> String
showError (UnknownError e) = e
showError (WontCompile es) = intercalate "\n" $ fmap errMsg es
showError (NotAllowed e)   = e
showError (GhcException e) = e
