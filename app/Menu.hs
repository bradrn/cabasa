{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Menu (addMenuHandlers) where

import Control.Monad (when, void, filterM)
import Data.Ix (range)
import Data.IORef
import Data.List (find)
import Data.Maybe (isJust)

import qualified CA.Format.MCell as MC
import Data.Text (pack)
import GI.Gtk
import Lens.Micro hiding (set)
import System.FilePath ((</>), takeExtension, takeBaseName, (-<.>))
import System.Directory (doesDirectoryExist, listDirectory)
import System.Process (callCommand)

import CA.Core (peek, evolve)
import CA.Types (Point(Point), Coord(Coord), Axis(X, Y), Universe)
import CA.Universe (render, fromList, size, clipInside, Bounds(..))
import qualified Utils as U
import Settings (getSetting')
import SettingsDialog
import qualified Types as T
import Paths_cabasa

addMenuHandlers :: T.Application -> IO ()
addMenuHandlers app = do
    _ <- on (app ^. T.drawMode) #activate $
        writeIORef (app ^. T.currentMode) T.DrawMode >> widgetSetSensitive (app ^. T.drawopts) True
    _ <- on (app ^. T.moveMode) #activate $
        writeIORef (app ^. T.currentMode) T.MoveMode >> widgetSetSensitive (app ^. T.drawopts) False
    _ <- on (app ^. T.selectMode) #activate $
        writeIORef (app ^. T.currentMode) T.SelectMode >> widgetSetSensitive (app ^. T.drawopts) False

    _ <- on (app ^. T.savePattern)   #activate $ savePattern app
    _ <- on (app ^. T.savePatternAs) #activate $ savePatternAs app
    _ <- on (app ^. T.openPattern)   #activate $ openPattern app

    _ <- on (app ^. T.about) #activate $ showAboutDialog app
    _ <- on (app ^. T.uman)  #activate $ showUserManual

    _ <- on (app ^. T.copyCanvas)    #activate $ copyCanvas app
    _ <- on (app ^. T.cutCanvas)     #activate $ cutCanvas  app
    _ <- on (app ^. T.pasteToCanvas) #activate $
        readIORef (app ^. T.selection) >>= \sel ->
            when (isJust sel) $ modifyIORef (app ^. T.currentMode) T.PastePendingMode

    _ <- on (app ^. T.changeGridSize) #activate $ changeGridSize app

    _ <- on (app ^. T.setRule)   #activate $ widgetShowAll (app ^. T.setRuleWindow)
    _ <- on (app ^. T.editSheet) #activate $ widgetShowAll (app ^. T.editSheetWindow)

    let when' p f = \x -> if p x then f x else x

    _ <- on (app ^. T.goFaster) #activate $ modifyDelay app (when' (>100) (`quot` 10))
    _ <- on (app ^. T.goSlower) #activate $ modifyDelay app (* 10)

    _ <- on (app ^. T.runSettings) #activate $ showSettingsDialog app

    _ <- on (app ^. T.quit) #activate $ mainQuit

    return ()

modifyDelay :: T.Application -> (Int -> Int) -> IO ()
modifyDelay app fn = do
    old <- readIORef (app ^. T.delay)
    let new = fn old
    writeIORef (app ^. T.delay) new
    labelSetText (app ^. T.delayLbl) $ pack $ show new

copyCanvas :: T.Application -> IO ()
copyCanvas app = readIORef (app ^. T.selection) >>= \case
    Nothing -> pure ()    -- Can't copy when there's no selection!
    Just ps -> doCopy ps app

cutCanvas :: T.Application -> IO ()
cutCanvas app = readIORef (app ^. T.selection) >>= \case
    Nothing -> pure ()    -- Can't cut when there's no selection!
    Just ps@(Point x1 y1, Point x2 y2) ->
        let lo = Point (min x1 x2) (min y1 y2)
            hi = Point (max x1 x2) (max y1 y2)
            inSelPs = range (lo,hi)
        in do
            doCopy ps app
            T.modifyState app $ \state ->
                let def = state ^. T.defaultVal
                in state & (T.currentPattern . _1) %~ evolve
                    (\p val ->
                         if p `elem` inSelPs
                         then def p
                         else peek p val)
            widgetQueueDraw (app ^. T.canvas)

doCopy :: (Point, Point)  -- ^ Selection
       -> T.Application -> IO ()
doCopy (Point x1 y1, Point x2 y2) app =
    T.modifyState app $ \state ->
        let (grid, _) = state ^. T.currentPattern
            (_, vals) = clipInside grid Bounds
                { boundsLeft   = min x1 x2
                , boundsRight  = max x1 x2
                , boundsTop    = min y1 y2
                , boundsBottom = max y1 y2
                }
        in state & T.clipboardContents .~ (Just $ fromList vals)

changeGridSize :: T.Application -> IO ()
changeGridSize app = do
    T.modifyStateM app $ \state -> do
        let (cols, rows) = size (state ^. (T.currentPattern . _1))
        adjustmentSetValue (app ^. T.newNumColsAdjustment) $ fromIntegral cols
        adjustmentSetValue (app ^. T.newNumRowsAdjustment) $ fromIntegral rows
        U.dialogRun' (app ^. T.newGridSizeDialog) >>= \case
           AnotherResponseType 1 -> do  -- OK button
               newCols <- floor <$> adjustmentGetValue (app ^. T.newNumColsAdjustment)
               newRows <- floor <$> adjustmentGetValue (app ^. T.newNumRowsAdjustment)
               return $ state & (T.currentPattern . _1) %~
                   (changeGridTo (Coord newCols, Coord newRows)
                                   (state ^. T.defaultVal))
           _ -> pure state  -- Don't change state
    widgetHide (app ^. T.newGridSizeDialog)
    widgetQueueDraw (app ^. T.canvas)
 where
   changeGridTo :: forall a. (Coord 'X, Coord 'Y) -> (Point -> a) -> Universe a -> Universe a
   changeGridTo (newCols, newRows) def u =
       let u' = render u
           (oldCols, oldRows) = size u
           dCols = newCols - oldCols
           dRows = newRows - oldRows

           withExtraRows = addRows dRows u'
           withExtraCols = addCols dCols withExtraRows
       in fromList withExtraCols
     where
         imap :: (Int -> x -> y) -> [x] -> [y]
         imap f = snd . foldr (\val (i,acc) -> (i+1, (f i val):acc)) (0, [])

         addCols :: Coord 'X -> [[a]] -> [[a]]
         addCols 0 = id
         addCols (Coord n)
             | n < 0 = fmap $ \row -> take (n + length row) row
             | otherwise = imap $ \i row ->
                   let firstNewCol = length row
                       newColNs = Coord @'X <$> [firstNewCol .. (firstNewCol + n - 1)]
                       newColVals = newColNs <&> \col -> def (Point col (Coord i))
                   in row ++ newColVals

         addRows :: Coord 'Y -> [[a]] -> [[a]]
         addRows 0 u' = u'
         addRows (Coord n) u'
             | n < 0 = take (n + length u') u'
             | otherwise =
                   let width = Coord @'X $ length (head u') - 1
                       firstNewRow = length u'
                       newRowNs = Coord @'Y <$> [firstNewRow .. (firstNewRow + n - 1)]
                       newRowVals = newRowNs <&> \row ->
                           [0..width] <&> \col -> def (Point col row)
                   in u' ++ newRowVals

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
            Left err -> U.showMessageDialog (app ^. T.window) MessageTypeError ButtonsTypeOk
                ("Could not decode file! The error was:\n" <> pack err)
                (const $ pure ())
            -- We rename one field to avoid shadowing Hint.Interop.rule
            Right MC.MCell{MC.rule=rule'mc, ..} -> do
                whenMaybeM rule'mc $ \rule' ->
                    whenM (maybe True (rule'==) <$> (app & T.getCurrentRuleName)) $ do
                        U.showMessageDialog (app ^. T.window) MessageTypeInfo ButtonsTypeYesNo
                            "This pattern is set to use a different rule to the rule currently loaded\nDo you want to change the rule to that specified in the pattern?"
                            $ \case
                            ResponseTypeYes -> do
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
                                    setTextBufferText (app ^. T.newRuleBuf) $ pack text
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
    findNewRule name = U.showMessageDialog (app ^. T.window) MessageTypeWarning ButtonsTypeYesNo
        ("Could not find the specified rule '" <> pack name <> "'.\nDo you want to find this rule manually?")
        $ \case
        ResponseTypeYes -> U.withFileDialogChoice (U.getRuleFileChooser app Nothing) FileChooserActionOpen $ const pure
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
        [ #transientFor := app ^. T.window
        , #programName  := "Cabasa"
        , #name         := "Cabasa"
        , #version      := "0.1"
        , #comments     := "An application for the simulation of arbitrary 2D cellular automata"
        , #authors      := ["Brad Neimann"]
        , #copyright    := "© Brad Neimann 2017-2018"
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
