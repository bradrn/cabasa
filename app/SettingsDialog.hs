{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module SettingsDialog (showSettingsDialog) where

import GI.Gtk
import Lens.Micro hiding (set)

import qualified Types as T
import qualified Types.Application as T
import Settings

showSettingsDialog :: T.Application -> IO ()
showSettingsDialog app = do
    _ <- getSetting' T.predefinedRulesDir app >>=
      fileChooserSetCurrentFolder (app ^. T.predefRulesDirChooser)

    _ <- getSetting' T.userRulesDir app >>=
      fileChooserSetCurrentFolder (app ^. T.userRulesDirChooser)

    _ <- getSetting (T.gridSize . _Just . _1) app >>= (fromIntegral <&> adjustmentSetValue (app ^. T.numColsAdjustment))
    _ <- getSetting (T.gridSize . _Just . _2) app >>= (fromIntegral <&> adjustmentSetValue (app ^. T.numRowsAdjustment))

    dialogRun (app ^. T.settingsWindow) >>= \case
       1 -> do  -- OK button
           _predefinedRulesDir <- fileChooserGetFilename (app ^. T.predefRulesDirChooser)
           _userRulesDir       <- fileChooserGetFilename (app ^. T.userRulesDirChooser)

           _gridSize <- fmap Just $
               (,) <$> (floor <$> adjustmentGetValue (app ^. T.numColsAdjustment))
                   <*> (floor <$> adjustmentGetValue (app ^. T.numRowsAdjustment))

           saveSettings (T.Settings {..}) app
       _ -> pure ()

    widgetHide (app ^. T.settingsWindow)
    
    return ()
