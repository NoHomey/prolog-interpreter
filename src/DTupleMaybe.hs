{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module DTupleMaybe (DTupleMaybe) where

import DTuple
import KeyedCollection

newtype DTupleMaybe a = DTupleMaybe {dtuple :: DTuple (Maybe a)}

instance KeyedCollection DTupleMaybe Digit where
    empty = cast $ fill Nothing
    find dt d = select d $ dtuple dt 
    insert dt d v = cast $ set d (dtuple dt) (Just v)

instance Functor DTupleMaybe where
    fmap f = cast . fmap (fmap f) . dtuple

cast :: DTuple (Maybe a) -> DTupleMaybe a
cast dt = DTupleMaybe {dtuple = dt}