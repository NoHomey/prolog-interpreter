{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module DTrie (DTrie) where

import DTuple
import DTupleMaybe
import KeyedCollection
import KeyToPath
import Trie

instance Empty Int where
    empty = 0 :: Int

instance Empty Integer where
    empty = 0 :: Integer

instance (Integral i, Empty i) => KeyToPath i Digit where
    toPath = toDigits
    append z d = (z * 10) + (fromDigit d)
    prepend d z = foldl append 0 (d:toDigits z)
    
type DTrie i a = Trie DTupleMaybe i Digit a