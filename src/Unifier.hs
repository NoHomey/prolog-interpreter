module Unifier (Unifier, unify) where

import PrologDataBase
import Control.Monad
import Data.Function
import Data.Maybe

type Unifier = [(Identifier, Term)]

type Equations = [(Term, Term)]

data UnificationStep = Delete | Decompose | Conflict | Check | Eliminate (Identifier, Term) 

vars :: Term -> [Identifier]
vars (Const _) = []
vars (Var x) = [x]
vars (Func f p) = concatMap vars p

step :: Equations -> Maybe (UnificationStep, Equations)
step [] = Nothing
step g@(e@(a, b):es) = if a == b 
                         then nextStep es Delete 
                         else case e of
                                  (Const _, Const _) -> Just (Check, g)
                                  (Const _, Var x) -> Just (Eliminate (x, a), es)
                                  (Func _ _, Var _) -> step $ (b, a):es
                                  (Var x, Func f p) -> Just $ if (x `elem` (vars b))
                                                                then (Check, g)
                                                                else (Eliminate (x, b), es)
                                  (Var x, t) -> Just (Eliminate (x, t), es)
                                  (Func f p1, Func h p2) -> if ((f /= h) || (((/=) `on` fArity) a b))
                                                              then Just (Conflict, g)
                                                              else nextStep ((zip p1 p2) ++ es) Decompose
                                  e -> do
                                         (reason, rs) <- step es
                                         return (reason, e:rs)
    where nextStep ng tag = let rest = step ng
                            in if isNothing rest
                                 then Just (tag, ng)
                                 else rest 

substitute :: Identifier -> Term -> Term -> Term
substitute x r t = case t of
                       (Var y) -> if x == y then r else t
                       (Const _) -> t
                       (Func f p) -> Func {funcSymbol = f, params = map (substitute x r) p}

unwrapVar :: Equations -> Unifier
unwrapVar = map (\(Var x, t) -> (x, t))

tryUnify :: Equations -> Maybe Unifier
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
                                                         return $ (x, (foldr (\(y, r) t' -> substitute y r t') t us)):us
                    
unify :: Atom -> Atom -> Maybe Unifier
unify a b = do
              guard $ ((==) `on` predSymbol) a b
              guard $ ((==) `on` pArity) a b
              tryUnify $ (zip `on` terms) a b