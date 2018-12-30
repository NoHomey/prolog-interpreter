module Trie (Trie, empty, insert, find) where

import qualified Prelude as P
import qualified Data.List as L
import Prelude hiding (find)
import Data.Maybe

data Trie k v = Node {value :: Maybe v, children :: [(k, Trie k v)]} deriving (Show)

empty :: Trie k v
empty = Node {value = Nothing, children = []}

findChild :: Eq k => (Trie k v) -> k -> Maybe (Trie k v)
findChild t x = case L.find ((x ==) . fst) $ children t of
                    Nothing -> Nothing
                    Just (_, c) -> Just c

getChild :: Ord k => (Trie k v) -> k -> Trie k v
getChild t x = case findChild t x of
                   Nothing -> empty
                   Just c -> c

sortedInsert :: Ord k => [(k, Trie k v)] -> k -> (Trie k v) -> [(k, Trie k v)]
sortedInsert [] x t = [(x, t)]
sortedInsert l@(c@(y, _):cs) x t = case compare y x of
                                     LT -> c:sortedInsert cs x t
                                     EQ -> (x, t):cs
                                     GT -> (x, t):l

insertIntoChildren :: Ord k => (Trie k v) -> k -> (Trie k v) -> [(k, Trie k v)]
insertIntoChildren t x c = sortedInsert (children t) x c 

insert :: Ord k => (Trie k v) -> [k] -> v -> (Trie k v)
insert t [] v = Node {value = Just v, children = children t}
insert t (x:xs) v = Node {value = value t, children = insertIntoChildren t x $ insert (getChild t x) xs v}

find :: Ord k => (Trie k v) -> [k] -> Maybe v
find t [] = value t
find t (x:xs) = case findChild t x of
                    Nothing -> Nothing
                    Just c -> find c xs