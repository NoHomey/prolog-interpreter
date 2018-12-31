{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module KeyToPath where

class KeyToPath k c where
    toPath :: k -> [c]

instance KeyToPath [a] a where
    toPath = id