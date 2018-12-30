module Main where

import qualified ParseTree as PT
import qualified PrologGrammar as PG
import qualified PrologDataBase as PDB
import qualified Unifier as U
import Data.Maybe

prog = "p(X). q(X) :- p(X). nat(z). nat(s(X)) :- nat(X). f(X1, X2) :- p(X1), q(X2). p(a). q(abc). f(abc, bc). g(suc(suc(X))) :- nat( s(X) ), f( abc, f(g(h(t))))."

rules :: [PG.E] -> PDB.PrologDataBase
rules str = PDB.prologDataBase $ fromJust $ PT.parse PG.prologGrammar (Just [' ', '\t', '\n']) PG.Start str

atom :: [PG.E] -> PDB.Atom
atom str = PDB.rhead $ head $ rules str

a = atom "p(g(X, V), X, a, V, s(T), T, h(c, d), c)."
b = atom "p(Y, s(Z), Z, s(W), W, b, h(c, d), c)."

main = do
    --print $ rules prog
    print $ U.unify a b