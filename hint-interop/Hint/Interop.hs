{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}

{-|
Module     : Hint.Interop
Copyright  : (c) Brad Neimann 2017-2018
License    : MIT
Maintainer : brad.neimann@hotmail.com

Types necessary for communication between Hint and the application.
-}
module Hint.Interop where

import CA

-- This datatype is split up into a record+existential wrapper so the state type
-- can be hidden and exposed as needed

-- | A record containing all the various bits and pieces of information
-- which the application needs to know to load a new rule.
data CAVals' t = CAVals'
    { _rule :: Universe t -> Rand StdGen t           -- ^ The rule itself
    , _states :: [t]                                 -- ^ The states which can be selected from the state selection menu
    , _defaultPattern :: Universe t                  -- ^ The default pattern displayed before anything has been edited
    , _state2color :: t -> (Double, Double, Double)  -- ^ A function to convert states into (red, green, blue) colours which are displayed on the grid
    , _encodeInt :: t -> Int                         -- ^ Encodes a state into an integer which __must__ be between 0 and 255. Used to save a pattern to a file.
    , _decodeInt :: Int -> t                         -- ^ Decodes a state from an integer between 0 and 255. Used to load a pattern from a file.
    }

-- | If we decide to use a custom state type for our CA, we can't use that type
-- outside of Hint, so this existential wrapper is used to \'hide' the state
-- type from the outside world.
data CAVals = forall t. Eq t => CAVals (CAVals' t)
