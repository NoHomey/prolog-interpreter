{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module AssocList (AssocList) where

import KeyedCollection

newtype AssocList k v = AssocList {assocList :: [(k, v)]}

instance Ord k =>  KeyedCollection (AssocList k) k where
    empty = cast []
    find al k = sortedFind (assocList al) k
    insert al k v = cast $ sortedInsert (assocList al) k v
    assoc = assocList

instance Functor (AssocList k) where
    fmap f = cast . map (fmap f) . assocList

cast :: [(k, v)] -> AssocList k v
cast al = AssocList {assocList = al}

sortedFind :: (Eq k) => [(k, v)] -> k -> Maybe v
sortedFind [] _  = Nothing
sortedFind ((x, v):rs) k = if x == k then Just v else sortedFind rs k

sortedInsert :: Ord k => [(k, v)] -> k -> v -> [(k, v)]
sortedInsert [] k v = [(k, v)]
sortedInsert l@(p@(x, _):ps) k v = case compare x k of
                                       LT -> p:sortedInsert ps k v
                                       EQ -> (k, v):ps
                                       GT -> (k, v):l