{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

module Handlers (addHandlers) where

import Control.Monad (when, (>=>))
import Data.Functor (($>))
import Data.Maybe (isJust)
import GHC.TypeLits (KnownNat)

import Control.Monad.Reader (ask)
import Data.GI.Base.Signals (SignalInfo, HaskellCallbackType, SignalProxy)
import qualified GI.Gtk
import GI.Gdk.Flags (EventMask(..))
import Lens.Micro

import Control.Monad.App
import qualified Types as T
import qualified Types.Application as T

import Control.Monad.App.Class
import qualified Menu
import qualified Canvas
import qualified ControlButtons

-- | A monad which allows adding event handlers to widgets. A
-- typeclass is used instead of plain 'App' to avoid the execution of
-- arbitrary 'IO', which is possible with 'App' due to its 'MonadIO'
-- instance.
class Monad m => AddHandler a m | m -> a where
    -- | Register an event handler with __no__ parameters.
    on :: ( HaskellCallbackType i ~ IO x
          , SignalInfo i
          , GI.Gtk.GObject o
          )
       => Lens' (T.Application a) o
       -> SignalProxy o i
       -> App a x
       -> m ()
    -- | Register an event handler with __one__ parameter.
    on2 :: ( HaskellCallbackType i ~ (r -> IO x)
           , SignalInfo i
           , GI.Gtk.GObject o
           )
        => Lens' (T.Application a) o
        -> SignalProxy o i
        -> (r -> App a x)
        -> m ()
    -- | Add supported events to a widget. This method is more
    -- GTK-specific than the others, and could be removed if Cabasa is
    -- moved to another GUI library.
    widgetAddEvents :: GI.Gtk.IsWidget o => Lens' (T.Application a) o -> [EventMask] -> m ()
    
instance AddHandler a (App a) where
    on w s h = ask >>= \app -> () <$ GI.Gtk.on (app ^. w) s (runApp h app)
    on2 w s h = ask >>= \app -> () <$ GI.Gtk.on (app ^. w) s (\r -> runApp (h r) app)
    widgetAddEvents w es = ask >>= \app -> GI.Gtk.widgetAddEvents (app ^. w) es

addHandlers :: (AddHandler a m) => m ()
addHandlers = do
    addMenuHandlers
    addCanvasHandlers
    addControlButtonsHandlers
 
addMenuHandlers :: (AddHandler a m) => m ()
addMenuHandlers = do
    on T.drawMode #activate   $ setMode T.DrawMode
    on T.moveMode #activate   $ setMode T.MoveMode
    on T.selectMode #activate $ setMode T.SelectMode

    on T.savePattern   #activate $ Menu.savePattern
    on T.savePatternAs #activate $ Menu.savePatternAs
    on T.openPattern   #activate $ Menu.openPattern

    on T.about #activate $ showAboutDialog
    on T.uman  #activate $ showUserManual

    on T.copyCanvas    #activate $ Menu.copyCanvas
    on T.cutCanvas     #activate $ Menu.cutCanvas
    on T.pasteToCanvas #activate $ getSelection >>= \sel -> when (isJust sel) setPastePending

    on T.changeGridSize #activate $ Menu.changeGridSize

    let when' p f = \x -> if p x then f x else x

    on T.goFaster #activate $ modifyDelay (when' (>100) (`quot` 10))
    on T.goSlower #activate $ modifyDelay (* 10)

    on T.quit #activate $ mainQuit

addCanvasHandlers :: (AddHandler a m) => m ()
addCanvasHandlers = do
    widgetAddEvents T.canvas
        [ EventMaskButtonPressMask
        , EventMaskButtonReleaseMask
        , EventMaskButtonMotionMask
        , EventMaskPointerMotionMask
        , EventMaskScrollMask
        ]

    on2 T.canvas #draw $ Canvas.drawCanvas

    on2 T.canvas #buttonPressEvent  $ (getMouseEventInfo . GtkMouseEvent) >=> Canvas.canvasMouseHandler True
    on2 T.canvas #motionNotifyEvent $ (getMouseEventInfo . GtkMouseEvent) >=> Canvas.canvasMouseHandler False
    on2 T.canvas #buttonReleaseEvent $ \_ -> eraseMousePointRecord $> True

    on2 T.canvas #scrollEvent $ getScrollEventInfo >=> Canvas.zoom

    on T.clearPattern #activate $ Canvas.clearPattern

    on T.clearSelection #activate $ setSelection Nothing

    return ()

addControlButtonsHandlers :: (AddHandler a m) => m ()
addControlButtonsHandlers = do
    on T.step  #clicked $ (saveRestorePattern >> ControlButtons.runGen)
    on T.run   #clicked $ ControlButtons.runButtonHandler
    on T.reset #clicked $ ControlButtons.resetButtonHandler
