{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Trie (Trie, empty, insert, find) where

import qualified KeyedCollection as KC
import qualified KeyToPath as KP

data Trie c k p v = Node {value :: Maybe v, children :: c (Trie c k p v)}

instance (Ord p, KP.KeyToPath k p, KC.KeyedCollection c p) => KC.KeyedCollection (Trie c k p) k where
    empty = empty
    find = find
    insert = insert

empty :: (KP.KeyToPath k p, KC.KeyedCollection c p) => Trie c k p v
empty = Node {value = Nothing, children = KC.empty}

findChild :: (Eq p, KP.KeyToPath k p, KC.KeyedCollection c p) => Trie c k p v -> p -> Maybe (Trie c k p v)
findChild t x = KC.find (children t) x

getChild :: (Eq p, KP.KeyToPath k p, KC.KeyedCollection c p) => Trie c k p v -> p -> Trie c k p v
getChild t x = case findChild t x of
                   Nothing -> empty
                   Just c -> c

insertIntoChildren :: (Ord p, KP.KeyToPath k p, KC.KeyedCollection c p) => Trie c k p v -> p -> Trie c k p v -> c (Trie c k p v)
insertIntoChildren t x ct = KC.insert (children t) x ct 

insertPath :: (Ord p, KP.KeyToPath k p, KC.KeyedCollection c p) => Trie c k p v -> [p] -> v -> Trie c k p v
insertPath t [] v = Node {value = Just v, children = children t}
insertPath t (x:xs) v = Node {value = value t, children = insertIntoChildren t x $ insertPath (getChild t x) xs v}

findPath :: (Eq p, KP.KeyToPath k p, KC.KeyedCollection c p) => Trie c k p v -> [p] -> Maybe v
findPath t [] = value t
findPath t (x:xs) = case findChild t x of
                        Nothing -> Nothing
                        Just c -> findPath c xs

insert :: (Ord p, KP.KeyToPath k p, KC.KeyedCollection c p) => Trie c k p v -> k -> v -> Trie c k p v
insert t k v = insertPath t (KP.toPath k) v

find :: (Eq p, KP.KeyToPath k p, KC.KeyedCollection c p) => Trie c k p v -> k -> Maybe v
find t k = findPath t (KP.toPath k)