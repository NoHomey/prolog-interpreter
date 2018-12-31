{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module DTrie where

import DTuple
import DTupleMaybe
import KeyedCollection
import KeyToPath
import Trie

instance Integral i => KeyToPath i Digit where
    toPath = toDigits
    
type DTrie i a = Trie DTupleMaybe i Digit a