{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.DTupleMaybe (DTupleMaybe) where

import Data.DTuple
import Data.KeyedCollection
import Data.Maybe

newtype DTupleMaybe a = DTupleMaybe {dtuple :: DTuple (Maybe a)}

instance KeyedCollection DTupleMaybe Digit where
    empty = cast $ fill Nothing
    find dt d = select d $ dtuple dt 
    insert dt d v = cast $ set d (dtuple dt) $ Just v
    assoc dt = [(d, fromJust mt) | d <- [(D0)..(D9)], let mt = find dt d, isJust mt]

instance Functor DTupleMaybe where
    fmap f = cast . fmap (fmap f) . dtuple

cast :: DTuple (Maybe a) -> DTupleMaybe a
cast dt = DTupleMaybe {dtuple = dt}