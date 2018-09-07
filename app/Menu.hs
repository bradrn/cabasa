{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Menu where

import Control.Monad (when, void)
import Data.IORef
import Data.List (find)

import qualified CA.Format.MCell as MC
import Graphics.UI.Gtk
import Lens.Micro hiding (set)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, combine, takeBaseName, (-<.>))
import System.Process (callCommand)

import qualified Common as C
import qualified Types as T
import Paths_cabasa

addMenuHandlers :: T.Application -> IO ()
addMenuHandlers app = do
    _ <- (app ^. T.drawMode) `on` menuItemActivated $
        writeIORef (app ^. T.currentMode) T.DrawMode >> widgetSetSensitive (app ^. T.drawopts) True
    _ <- (app ^. T.moveMode) `on` menuItemActivated $
        writeIORef (app ^. T.currentMode) T.MoveMode >> widgetSetSensitive (app ^. T.drawopts) False

    _ <- (app ^. T.savePatternAs) `on` menuItemActivated $ savePattern app
    _ <- (app ^. T.openPattern)   `on` menuItemActivated $ openPattern app

    _ <- (app ^. T.about) `on` menuItemActivated $ showAboutDialog app
    _ <- (app ^. T.uman)  `on` menuItemActivated $ showUserManual

    _ <- (app ^. T.setRule) `on` menuItemActivated $ widgetShowAll (app ^. T.setRuleWindow)

    _ <- (app ^. T.quit) `on` menuItemActivated $ mainQuit

    return ()

savePattern :: T.Application -> IO ()
savePattern app = void $
    C.withFileDialogChoice (C.getPatternFileChooser app) FileChooserActionSave $ const $ \fName ->
        T.withState app $ \state -> do
            ruleName <- readIORef (app ^. T.currentRuleName)
            let p = state ^. T.currentPattern . _1
                ss = state ^. T.states
                encode = state ^. T.encodeInt
                mc = MC.MCell { MC.game = Just MC.SpecialRules
                              , MC.rule = ruleName
                              , MC.speed = Nothing
                              , MC.ccolors = Just (length ss)
                              , MC.coloring = Nothing
                              , MC.wrap = Just True
                              , MC.palette = Nothing
                              , MC.description = Nothing
                              , MC.universe = encode <$> p
                              , MC.diversities = []
                              }
            writeFile (fName -<.> "mcl") $ MC.encodeMCell mc

openPattern :: T.Application -> IO ()
openPattern app = void $
    C.withFileDialogChoice (C.getPatternFileChooser app) FileChooserActionOpen $ const $ \fName -> do
        pat <- readFile fName
        case MC.decodeMCell pat of
            Left err -> C.showMessageDialog (Just $ app ^. T.window) MessageError ButtonsOk
                ("Could not decode file! The error was:\n" ++ err)
                (const $ pure ())
            -- We rename one field to avoid shadowing Hint.Interop.rule
            Right MC.MCell{MC.rule=rule'mc, ..} -> do
                case rule'mc of
                    Nothing -> return ()
                    Just rule' -> (maybe True (rule'==) <$> readIORef (app ^. T.currentRuleName)) >>= flip when (do
                        C.showMessageDialog (Just $ app ^. T.window) MessageInfo ButtonsYesNo
                            "This pattern is set to use a different rule to the rule currently loaded\nDo you want to change the rule to that specified in the pattern?"
                            $ \case
                            ResponseYes ->
                                let findNewRule = C.showMessageDialog (Just $ app ^. T.window) MessageWarning ButtonsYesNo
                                        ("Could not find the specified rule '" ++ rule' ++ "'.\nDo you want to find this rule manually?")
                                        $ \case
                                        ResponseYes -> C.withFileDialogChoice (C.getRuleFileChooser app Nothing) FileChooserActionOpen $ const pure
                                        _ -> return Nothing
                                in do
                                    dataFile <- getDataFileName "Rules"
                                    listDirectory dataFile
                                      >>= pure . find ((rule'==) . takeBaseName)
                                      >>= (\case { Just a -> pure $ Just a; Nothing -> findNewRule}) >>= \case
                                        Nothing -> return ()
                                        Just (combine dataFile -> file) -> do
                                            text <- readFile $ file
                                            let ruleType = case (takeExtension file) of
                                                    ".hs"  -> T.Hint
                                                    ".alp" -> T.ALPACA
                                                    _ -> T.ALPACA -- guess
                                            C.setCurrentRule app (Just rule') text ruleType
                                            -- Set this rule's text in the Set Rule dialog
                                            textBufferSetText (app ^. T.newRuleBuf) text
                            _ -> return ())

                T.modifyState app $ \state ->
                    let fn = state ^. T.decodeInt
                    in state & (T.currentPattern . _1) .~ (fn <$> universe)
                widgetQueueDraw (app ^. T.canvas)

showAboutDialog :: T.Application -> IO ()
showAboutDialog app = do
    a <- aboutDialogNew
    set a
        [ windowTransientFor     := app ^. T.window
        , aboutDialogProgramName := "Cabasa"
        , aboutDialogName        := "Cabasa"
        , aboutDialogVersion     := "0.1"
        , aboutDialogComments    := "An application for the simulation of arbitrary 2D cellular automata"
        , aboutDialogAuthors     := ["Brad Neimann"]
        , aboutDialogCopyright   := "© Brad Neimann 2017-2018"
        ]
    _ <- dialogRun a
    widgetDestroy a

showUserManual :: IO ()
showUserManual = do
    location <- getDataFileName "doc/UserManual.pdf"
#ifdef mingw32_HOST_OS
    callCommand $ "start " ++ location
#elif defined linux_HOST_OS
    callCommand $ "xdg-open" ++ location
#elif defined darwin_HOST_OS
    callCommand $ "open"     ++ location
#else
    showMessageDialog (Just window) MessageError ButtonsOk
        "ERROR: Could not figure out how to open manual on your system.\nThis is a bug - please report it on the project website" pure
#endif
