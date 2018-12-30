module DTrie (DTrie, empty, insert, find) where

import qualified Prelude as P
import qualified Data.List as L
import Prelude hiding (find)
import Data.Maybe
import DTuple

data DTrie a = Node {value :: Maybe a, children :: DTuple (Maybe (DTrie a))}
type Children a = DTuple (Maybe (DTrie a))

empty :: DTrie a
empty = Node {value = Nothing, children = fill Nothing}

findChild :: DTrie a -> Digit -> Maybe (DTrie a)
findChild t d = select d $ children t

getChild :: DTrie a -> Digit -> DTrie a
getChild t d = case findChild t d of
                   Nothing -> empty
                   Just c -> c

insertDigitoChildren :: DTrie a -> Digit -> DTrie a -> Children a
insertDigitoChildren t d c = set d (children t) (Just c) 

insertD :: DTrie a -> [Digit] -> a -> DTrie a
insertD t [] v = Node {value = Just v, children = children t}
insertD t (x:xs) v = Node {value = value t, children = insertDigitoChildren t x $ insertD (getChild t x) xs v}

findD :: DTrie a -> [Digit] -> Maybe a
findD t [] = value t
findD t (x:xs) = case findChild t x of
                    Nothing -> Nothing
                    Just c -> findD c xs

insert :: Integral i => DTrie a -> i -> a -> DTrie a
insert t n v = insertD t (toDigits n) v

find :: Integral i => DTrie a -> i -> Maybe a
find t n = findD t (toDigits n)