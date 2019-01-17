{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.KeyedCollection where

class KeyedCollection c k | c -> k where
    empty :: c v
    find :: Eq k => c v -> k -> Maybe v
    insert :: c v -> k -> v -> c v
    assoc :: c v -> [(k, v)]