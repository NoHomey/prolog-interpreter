module Prolog.Types (
    Term(..),
    Terms,
    Atom(..),
    Atoms,
    Query,
    Rule(..),
    Rules,
    isVar,
    isFunc,
    identifier
) where

import Data.Bifunctor
import Trifunctor
import FunctorM

data Term s v = Var v | Func {funcSymbol :: s, parms :: Terms s v} deriving (Eq)

type Terms s v = [Term s v]

data Atom p s v = Atom {predSymbol :: p, terms :: Terms s v} deriving (Eq)

type Atoms p s v = [Atom p s v]

type Query p s v = Atoms p s v

data Rule p s v = Rule {ruleHead :: Atom p s v, body :: Atoms p s v} deriving (Eq)

type Rules p s v = [Rule p s v]

instance Bifunctor Term where
    bimap f g (Var x)     = Var $ g x
    bimap f g (Func s ps) = Func (f s) $ map (bimap f g) ps

instance Functor (Term s) where
    fmap = bimap id

instance Trifunctor Atom where
    trimap h f g (Atom p ts) = Atom (h p) $ map (bimap f g) ts

instance Bifunctor (Atom p) where
    bimap = trimap id
    
instance Functor (Atom p s) where
    fmap = bimap id

instance Trifunctor Rule where
    trimap h f g (Rule rh rs) = let t = trimap h f g
                                in Rule (t rh) $ map t rs  

instance Bifunctor (Rule p) where
    bimap = trimap id
    
instance Functor (Rule p s) where
    fmap = bimap id

instance BifunctorM Term where
    bimapM f g (Var x)     = do
                               x' <- g x
                               return $ Var x'
    bimapM f g (Func s ps) = do
                               s' <- f s
                               ps' <- mapM (bimapM f g) ps
                               return $ Func s' ps'

instance FunctorM (Term s) where
    fmapM = bimapM return

instance TrifunctorM Atom where
    trimapM h f g (Atom p ts) = do
                                  p' <- h p
                                  ts' <- mapM (bimapM f g) ts
                                  return $ Atom p' ts'

instance BifunctorM (Atom p) where
    bimapM = trimapM return
    
instance FunctorM (Atom p s) where
    fmapM = bimapM return

instance TrifunctorM Rule where
    trimapM h f g (Rule rh rs) = do
                                   let t = trimapM h f g
                                   rh' <- t rh
                                   rs' <- mapM t rs
                                   return $ Rule rh' rs'  

instance BifunctorM (Rule p) where
    bimapM = trimapM return
    
instance FunctorM (Rule p s) where
    fmapM = bimapM return

instance (Show s, Show v) => Show (Term s v) where
    show (Var id) = show id
    show f = let ps = parms f
             in (show $ funcSymbol f) ++ if null ps then "" else show ps

instance (Show p, Show s, Show v) => Show (Atom p s v) where
    show a = (show $ predSymbol a) ++ (show $ terms a)

instance (Show p, Show s, Show v) => Show (Rule p s v) where
    show r = let rs = body r
             in (show $ ruleHead r) ++ if null rs then "" else (" <- " ++ (show rs))

isVar :: Term s v -> Bool
isVar (Var _) = True
isVar _       = False

isFunc :: Term s v -> Bool
isFunc (Func _ _) = True
isFund _          = False

identifier :: Term s v -> Either s v
identifier (Var x) = Right x
identifier (Func f _) = Left f