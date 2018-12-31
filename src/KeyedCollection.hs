{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module KeyedCollection where

class KeyedCollection c k | c -> k where
    empty :: c v
    find :: Eq k => c v -> k -> Maybe v
    insert :: Ord k => c v -> k -> v -> c v