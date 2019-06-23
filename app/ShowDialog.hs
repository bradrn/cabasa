{-# LANGUAGE OverloadedLabels #-}

-- This is a small module containing only one convenience function.
-- Usually a function such as this would be located in Utils.hs, but
-- here this would cause a circular dependency between Utils.hs and
-- Settings.hs, as Settings would use Utils.showMessageDialog but
-- Utils would require one of the functions from Settings. This
-- module is used as a circuit-breaker so this situation does not
-- occur.
module ShowDialog (showMessageDialog, dialogRun') where

import Control.Monad.IO.Class (MonadIO)
import GHC.Stack (HasCallStack)

import GI.Gtk
import Data.Text (Text)

showMessageDialog :: Window -> MessageType -> ButtonsType -> Text -> (ResponseType -> IO a) -> IO a
showMessageDialog window level buttons message fn = do
    d <- new MessageDialog
        [ #transientFor := window
        , #messageType  := level
        , #buttons      := buttons
        , #text         := message
        ]
    response <- dialogRun' d
    #destroy d
    fn response

dialogRun' :: (HasCallStack, MonadIO m, IsDialog a) => a -> m ResponseType
dialogRun' = fmap (toEnum.fromIntegral) . dialogRun
