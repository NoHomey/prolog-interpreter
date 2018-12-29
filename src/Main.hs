module Main where

import ParseTree
import PrologGrammar
import PrologDataBase
import Data.Maybe

prog = "p(X). q(X) :- p(X). nat(z). nat(s(X)) :- nat(X). f(X1, X2) :- p(X1), q(X2). p(a). q(abc). f(abc, bc). g(suc(suc(X))) :- nat( s(X) ), f( abc, f(g(h(t))))."

main = do
    print $ prologDataBase $ fromJust $ parse prologGrammar (Just [' ', '\t', '\n']) Start prog