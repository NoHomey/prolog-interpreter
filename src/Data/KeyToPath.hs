{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.KeyToPath (
    KeyToPath,
    toPath,
    fromPath
) where

class KeyToPath k s where
    toPath :: k -> [s]
    fromPath :: [s] -> k

instance KeyToPath [a] a where
    toPath = id
    fromPath = id