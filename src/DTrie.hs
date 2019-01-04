{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module DTrie (DTrie) where

import DTuple
import DTupleMaybe
import KeyedCollection
import KeyToPath
import Trie

instance Integral i => KeyToPath i Digit where
    promote d = fromDigit d
    toPath = toDigits
    append z d = (z * 10) + (fromDigit d)
    prepend d z = foldl append 0 (d:toDigits z)
    
type DTrie i a = Trie DTupleMaybe i Digit a