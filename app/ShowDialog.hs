-- This is a small module containing only one convenience function.
-- Usually a function such as this would be located in Utils.hs, but
-- here this would cause a circular dependency between Utils.hs and
-- Settings.hs, as Settings would use Utils.showMessageDialog but
-- Utils would require one of the functions from Settings. This
-- module is used as a circuit-breaker so this situation does not
-- occur.
module ShowDialog (showMessageDialog) where

import Graphics.UI.Gtk

showMessageDialog :: Maybe Window -> MessageType -> ButtonsType -> String -> (ResponseId -> IO a) -> IO a
showMessageDialog window level buttons message fn = do
    d <- messageDialogNew window [DialogModal] level buttons message
    result <- dialogRun d
    widgetDestroy d
    fn result
