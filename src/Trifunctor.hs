module Trifunctor where

class Trifunctor t where
    trimap :: (a -> a') -> (b -> b') -> (c -> c') -> t a b c -> t a' b' c'

instance Trifunctor (,,) where
    trimap f g h (x, y, z) = (f x, g y, h z)

first :: Trifunctor t => (a -> b) -> t a c d -> t b c d
first f = trimap f id id

second :: Trifunctor t => (b -> c) -> t a b d -> t a c d
second f = trimap id f id

third :: Trifunctor t => (c -> d) -> t a b c -> t a b d
third = trimap id id