{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Trie (Trie, empty, insert, find, KC.assoc) where

import qualified Data.KeyedCollection as KC
import qualified Data.KeyToPath as KP
import Control.Monad
import Data.Maybe
import Data.Bifunctor

data Trie c k s v = Node {value :: Maybe v, children :: c (Trie c k s v)}

instance (Ord s, KP.KeyToPath k s, KC.KeyedCollection c s) => KC.KeyedCollection (Trie c k s) k where
    empty = empty
    find = find
    insert = insert
    assoc = map (bimap KP.fromPath id) . removeRoot . assocTrie
        where removeRoot (([],_):al) = al
              removeRoot al          = al

instance (Functor c) => Functor (Trie c k s) where
    fmap f t = Node (fmap f $ value t) $ fmap (fmap f) $ children t

empty :: (KP.KeyToPath k s, KC.KeyedCollection c s) => Trie c k s v
empty = Node Nothing KC.empty

findChild :: (Eq s, KP.KeyToPath k s, KC.KeyedCollection c s) => Trie c k s v -> s -> Maybe (Trie c k s v)
findChild = KC.find . children

getChild :: (Eq s, KP.KeyToPath k s, KC.KeyedCollection c s) => Trie c k s v -> s -> Trie c k s v
getChild t x = maybe empty id $ findChild t x

insertIntoChildren :: (Ord s, KP.KeyToPath k s, KC.KeyedCollection c s) => Trie c k s v -> s -> Trie c k s v -> c (Trie c k s v)
insertIntoChildren = KC.insert . children 

insertPath :: (Ord s, KP.KeyToPath k s, KC.KeyedCollection c s) => Trie c k s v -> [s] -> v -> Trie c k s v
insertPath t []     v = Node (Just v) $ children t
insertPath t (x:xs) v = Node (value t) $ insertIntoChildren t x $ insertPath (getChild t x) xs v

findPath :: (Eq s, KP.KeyToPath k s, KC.KeyedCollection c s) => Trie c k s v -> [s] -> Maybe v
findPath t []     = value t
findPath t (x:xs) = join $ fmap (\c -> findPath c xs) $ findChild t x

insert :: (Ord s, KP.KeyToPath k s, KC.KeyedCollection c s) => Trie c k s v -> k -> v -> Trie c k s v
insert t = insertPath t . KP.toPath

find :: (Eq s, KP.KeyToPath k s, KC.KeyedCollection c s) => Trie c k s v -> k -> Maybe v
find t = findPath t . KP.toPath

assocTrie :: (KP.KeyToPath k s, KC.KeyedCollection c s) => Trie c k s v -> [([s], v)]
assocTrie t = let cs = [(s:p, v) | (s, ct) <- KC.assoc $ children t, (p, v) <- assocTrie ct]
              in maybe cs (\v -> ([], v):cs) $ value t