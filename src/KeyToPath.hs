{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module KeyToPath (
    Empty,
    KeyToPath,
    empty,
    toPath,
    append,
    prepend
) where

import Empty

class Empty k => KeyToPath k c where
    toPath :: k -> [c]
    append :: k -> c -> k
    prepend :: c -> k -> k

instance KeyToPath [a] a where
    toPath = id
    append l x = l ++ [x]
    prepend x l = x:l