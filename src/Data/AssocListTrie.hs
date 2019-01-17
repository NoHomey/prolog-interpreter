{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.AssocListTrie (AssocListTrie) where

import Data.Trie
import Data.AssocList
    
type AssocListTrie s v = Trie (AssocList s) [s] s v