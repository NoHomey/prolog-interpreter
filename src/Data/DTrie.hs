{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.DTrie (DTrie) where

import Data.DTuple
import Data.DTupleMaybe
import Data.KeyToPath
import Data.Trie

instance Integral i => KeyToPath i Digit where
    toPath = toDigits
    fromPath = fromDigits
    
type DTrie i a = Trie DTupleMaybe i Digit a