{-# LANGUAGE FlexibleInstances #-}

module Empty where

class Empty t where
    empty :: t

instance Empty [a] where
    empty = []