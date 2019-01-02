module Unifier (
     Unifier,
     empty,
     vars,
     substitute,
     applySubstitution,
     substitution,
     compose,
     unify
) where

import PrologRules
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe

type Unifier s v = [(v, Term s v)]

type Equations s v = [(Term s v, Term s v)]

data UnificationStep s v = Delete | Decompose | Conflict | Check | Eliminate (v, Term s v) 

empty :: Unifier s v
empty = []

vars :: Term s v -> [v]
vars (Const _) = []
vars (Var x) = [x]
vars (Func f p) = concatMap vars p

step :: (Eq s, Eq v) => Equations s v -> Maybe (UnificationStep s v, Equations s v)
step [] = Nothing
step g@(e@(a, b):es) = if a == b 
                         then nextStep es Delete 
                         else case e of
                                  (Const _, Const _) -> Just (Check, g)
                                  (Const _, Func _ _) -> Just (Check, g)
                                  (Func _ _, Const _) -> Just (Check, g)
                                  (Const _, Var x) -> Just (Eliminate (x, a), es)
                                  (Func _ _, Var _) -> step $ (b, a):es
                                  (Var x, Func f p) -> Just $ if x `elem` (vars b)
                                                                then (Check, g)
                                                                else (Eliminate (x, b), es)
                                  (Var x, t) -> Just (Eliminate (x, t), es)
                                  (Func f p1, Func h p2) -> if (f /= h) || (((/=) `on` fArity) a b)
                                                              then Just (Conflict, g)
                                                              else nextStep ((zip p1 p2) ++ es) Decompose
    where nextStep ng tag = let rest = step ng
                            in if isNothing rest
                                 then Just (tag, ng)
                                 else rest 

unwrapVar :: Equations s v -> Unifier s v
unwrapVar = map (\(Var x, t) -> (x, t))

substitute :: (Eq v) => v -> Term s v -> Term s v -> Term s v
substitute x r t = tmap (\y -> if x == y then r else (Var y)) t

applySubstitution :: (v -> Maybe (Term s v)) -> Term s v -> Term s v
applySubstitution s = tmap (\x -> let mt = s x in if isJust mt then fromJust mt else (Var x))

substitution :: (Eq v) => Unifier s v -> v -> Maybe (Term s v)
substitution u v = do
                     r <- find ((v ==) . fst) u
                     return $ snd r

compose :: (Eq v) => Unifier s v -> Unifier s v -> Unifier s v
compose uf ug = (map (fmap (applySubstitution (substitution uf))) ug) ++ (filter (isNothing . substitution ug . fst) uf)

tryUnify :: (Eq s, Eq v) => Equations s v -> Maybe (Unifier s v)
tryUnify g = let s = step g
             in if isNothing s
                  then Just $ unwrapVar g
                  else case fromJust s of
                           (Delete, g') -> Just $ unwrapVar g'
                           (Decompose, g') -> Just $ unwrapVar g'
                           (Conflict, _) -> Nothing
                           (Check, _) -> Nothing
                           (Eliminate (x, t), g') -> do
                                                       us <- tryUnify $ map (\(y, r) -> (y, substitute x t r)) g'
                                                       return $ (x, applySubstitution (substitution us) t):us
                    
unify :: (Eq p, Eq s, Eq v) => Atom p s v -> Atom p s v -> Maybe (Unifier s v)
unify a b = do
              guard $ ((==) `on` predSymbol) a b
              guard $ ((==) `on` pArity) a b
              tryUnify $ (zip `on` terms) a b