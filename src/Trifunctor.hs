module Trifunctor where

class Trifunctor t where
    trimap :: (a -> a') -> (b -> b') -> (c -> c') -> t a b c -> t a' b' c'

instance Trifunctor (,,) where
    trimap f g h (x, y, z) = (f x, g y, h z)