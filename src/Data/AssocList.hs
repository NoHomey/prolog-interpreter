{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.AssocList (AssocList) where

import Data.KeyedCollection

newtype AssocList k v = AssocList {assocList :: [(k, v)]}

instance Ord k =>  KeyedCollection (AssocList k) k where
    empty = AssocList []
    find = sortedFind . assocList
    insert al k v = AssocList $ sortedInsert (assocList al) k v
    assoc = assocList

instance Functor (AssocList k) where
    fmap f = AssocList . map (fmap f) . assocList

sortedFind :: (Eq k) => [(k, v)] -> k -> Maybe v
sortedFind []          _ = Nothing
sortedFind ((x, v):rs) k = if x == k then Just v else sortedFind rs k

sortedInsert :: Ord k => [(k, v)] -> k -> v -> [(k, v)]
sortedInsert []              k v = [(k, v)]
sortedInsert l@(p@(x, _):ps) k v = case x `compare` k of
                                       LT -> p:sortedInsert ps k v
                                       EQ -> (k, v):ps
                                       GT -> (k, v):l