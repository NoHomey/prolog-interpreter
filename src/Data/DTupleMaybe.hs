{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.DTupleMaybe (DTupleMaybe) where

import Data.DTuple
import Data.KeyedCollection
import Data.Maybe

newtype DTupleMaybe a = DTupleMaybe {dtuple :: DTuple (Maybe a)}

instance KeyedCollection DTupleMaybe Digit where
    empty = DTupleMaybe $ fill Nothing
    find dt d = select d $ dtuple dt 
    insert dt d v = DTupleMaybe $ set d (dtuple dt) $ Just v
    assoc dt = [(d, fromJust mt) | d <- [(D0)..(D9)], let mt = find dt d, isJust mt]

instance Functor DTupleMaybe where
    fmap f = DTupleMaybe . fmap (fmap f) . dtuple