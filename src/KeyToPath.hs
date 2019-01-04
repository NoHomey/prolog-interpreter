{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module KeyToPath (
    KeyToPath,
    promote,
    toPath,
    append,
    prepend
) where

class KeyToPath k c where
    promote :: c -> k
    toPath :: k -> [c]
    append :: k -> c -> k
    prepend :: c -> k -> k

instance KeyToPath [a] a where
    promote x = [x]
    toPath = id
    append l x = l ++ [x]
    prepend x l = x:l