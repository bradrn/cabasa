{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Menu (addMenuHandlers) where

import Control.Monad (when, void, filterM)
import Data.IORef
import Data.List (find)

import qualified CA.Format.MCell as MC
import Graphics.UI.Gtk
import Lens.Micro hiding (set)
import System.FilePath ((</>), takeExtension, takeBaseName, (-<.>))
import System.Directory (doesDirectoryExist, listDirectory)
import System.Process (callCommand)

import CA.Types (Point(Point))
import CA.Universe (fromList, clipInside, Bounds(..))
import qualified Utils as U
import Settings (getSetting')
import SettingsDialog
import qualified Types as T
import Paths_cabasa

addMenuHandlers :: T.Application -> IO ()
addMenuHandlers app = do
    _ <- (app ^. T.drawMode) `on` menuItemActivated $
        writeIORef (app ^. T.currentMode) T.DrawMode >> widgetSetSensitive (app ^. T.drawopts) True
    _ <- (app ^. T.moveMode) `on` menuItemActivated $
        writeIORef (app ^. T.currentMode) T.MoveMode >> widgetSetSensitive (app ^. T.drawopts) False
    _ <- (app ^. T.selectMode) `on` menuItemActivated $
        writeIORef (app ^. T.currentMode) T.SelectMode >> widgetSetSensitive (app ^. T.drawopts) False

    _ <- (app ^. T.savePattern)   `on` menuItemActivated $ savePattern app
    _ <- (app ^. T.savePatternAs) `on` menuItemActivated $ savePatternAs app
    _ <- (app ^. T.openPattern)   `on` menuItemActivated $ openPattern app

    _ <- (app ^. T.about) `on` menuItemActivated $ showAboutDialog app
    _ <- (app ^. T.uman)  `on` menuItemActivated $ showUserManual

    _ <- (app ^. T.copyCanvas)    `on` menuItemActivated $ copyCanvas app
    _ <- (app ^. T.pasteToCanvas) `on` menuItemActivated $
        modifyIORef (app ^. T.currentMode) T.PastePendingMode

    _ <- (app ^. T.setRule)   `on` menuItemActivated $ widgetShowAll (app ^. T.setRuleWindow)
    _ <- (app ^. T.editSheet) `on` menuItemActivated $ widgetShowAll (app ^. T.editSheetWindow)

    let when' p f = \x -> if p x then f x else x

    _ <- (app ^. T.goFaster) `on` menuItemActivated $ modifyDelay app (when' (>100) (`quot` 10))
    _ <- (app ^. T.goSlower) `on` menuItemActivated $ modifyDelay app (* 10)

    _ <- (app ^. T.runSettings) `on` menuItemActivated $ showSettingsDialog app

    _ <- (app ^. T.quit) `on` menuItemActivated $ mainQuit

    return ()

modifyDelay :: T.Application -> (Int -> Int) -> IO ()
modifyDelay app fn = do
    old <- readIORef (app ^. T.delay)
    let new = fn old
    writeIORef (app ^. T.delay) new
    labelSetText (app ^. T.delayLbl) $ show new

copyCanvas :: T.Application -> IO ()
copyCanvas app = readIORef (app ^. T.selection) >>= \case
    Nothing -> pure ()    -- Can't copy when there's no selection!
    Just (Point x1 y1, Point x2 y2) ->
        T.modifyState app $ \state ->
            let (grid, _) = state ^. T.currentPattern
                (_, vals) = clipInside grid Bounds
                    { boundsLeft   = min x1 x2
                    , boundsRight  = max x1 x2
                    , boundsTop    = min y1 y2
                    , boundsBottom = max y1 y2
                    }
            in state & T.clipboardContents .~ (Just $ fromList vals)

savePattern :: T.Application -> IO ()
savePattern app =
    readIORef (app ^. T.currentPatternPath) >>= \case
        Nothing   -> savePatternAs app
        Just path -> writeCurrentPattern app path

savePatternAs :: T.Application -> IO ()
savePatternAs app = void $
    U.withFileDialogChoice (U.getPatternFileChooser app) FileChooserActionSave $
        const $ writeCurrentPattern app

writeCurrentPattern :: T.Application -> FilePath -> IO ()
writeCurrentPattern app fName = 
    T.withState app $ \state -> do
        ruleName <- app & T.getCurrentRuleName
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
            path = fName -<.> "mcl"
        writeFile path $ MC.encodeMCell mc
        writeIORef (app ^. T.currentPatternPath) $ Just path

openPattern :: T.Application -> IO ()
openPattern app = void $
    U.withFileDialogChoice (U.getPatternFileChooser app) FileChooserActionOpen $ const $ \fName -> do
        pat <- readFile fName
        case MC.decodeMCell pat of
            Left err -> U.showMessageDialog (Just $ app ^. T.window) MessageError ButtonsOk
                ("Could not decode file! The error was:\n" ++ err)
                (const $ pure ())
            -- We rename one field to avoid shadowing Hint.Interop.rule
            Right MC.MCell{MC.rule=rule'mc, ..} -> do
                whenMaybeM rule'mc $ \rule' ->
                    whenM (maybe True (rule'==) <$> (app & T.getCurrentRuleName)) $ do
                        U.showMessageDialog (Just $ app ^. T.window) MessageInfo ButtonsYesNo
                            "This pattern is set to use a different rule to the rule currently loaded\nDo you want to change the rule to that specified in the pattern?"
                            $ \case
                            ResponseYes -> do
                                predefDir <- getSetting' T.predefinedRulesDir app
                                userDir   <- getSetting' T.userRulesDir       app

                                rules <- listDirectories [userDir, predefDir]
                                let ruleLocation = find ((rule'==) . takeBaseName) rules
                                ruleLocationFinal <- findIfNotSelected rule' ruleLocation
                                whenMaybeM ruleLocationFinal $ \file -> do
                                    text <- readFile $ file
                                    let ruleType = case (takeExtension file) of
                                            ".hs"  -> T.Hint
                                            ".alp" -> T.ALPACA
                                            _ -> T.ALPACA -- guess
                                    U.setCurrentRule app (Just file) text ruleType
                                    -- Set this rule's text in the Set Rule dialog
                                    textBufferSetText (app ^. T.newRuleBuf) text
                            _ -> return ()

                T.modifyState app $ \state ->
                    let fn = state ^. T.decodeInt
                    in state & (T.currentPattern . _1) .~ (fn <$> universe)
                writeIORef (app ^. T.currentPatternPath) $ Just pat
                widgetQueueDraw (app ^. T.canvas)
  where
    whenM :: IO Bool -> IO () -> IO ()
    whenM c i = c >>= flip when i

    whenMaybeM :: Applicative t => Maybe a -> (a -> t ()) -> t ()
    whenMaybeM Nothing _ = pure ()
    whenMaybeM (Just a) f = f a

    findIfNotSelected :: String -> Maybe FilePath -> IO (Maybe FilePath)
    findIfNotSelected name Nothing = findNewRule name
    findIfNotSelected _    (Just r) = pure $ Just r

    findNewRule :: String -> IO (Maybe FilePath)
    findNewRule name = U.showMessageDialog (Just $ app ^. T.window) MessageWarning ButtonsYesNo
        ("Could not find the specified rule '" ++ name ++ "'.\nDo you want to find this rule manually?")
        $ \case
        ResponseYes -> U.withFileDialogChoice (U.getRuleFileChooser app Nothing) FileChooserActionOpen $ const pure
        _ -> return Nothing

    listDirectories :: [FilePath] -> IO [FilePath]
    listDirectories ds =
        filterM doesDirectoryExist ds
        >>= traverse listDirectoryWithPath
        >>= (pure . concat)
      where
        listDirectoryWithPath dir = (fmap . fmap) (dir </>) $ listDirectory dir

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
