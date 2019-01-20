module FunctorM where

class FunctorM t where
    fmapM :: Monad m => (a -> m b) -> t a -> m (t b)

class BifunctorM t where
    bimapM :: Monad m => (a -> m c) -> (b -> m d) -> t a b -> m (t c d)

class TrifunctorM t where
    trimapM :: Monad m => (a -> m a') -> (b -> m b') -> (c -> m c') -> t a b c -> m (t a' b' c') 