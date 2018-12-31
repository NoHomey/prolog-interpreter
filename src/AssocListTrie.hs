{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module AssocListTrie where

import KeyedCollection
import KeyToPath
import Trie
import AssocList
    
type AssocListTrie l v = Trie (AssocList l) [l] l v