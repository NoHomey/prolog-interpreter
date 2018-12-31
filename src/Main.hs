module Main where

import qualified ParseTree as PT
import qualified PrologGrammar as PG
import qualified PrologRules as PRs
import qualified Unifier as U
import qualified DTrie as DT
import qualified Trie as T
import qualified AssocListTrie as ALT
import Data.Maybe

prog = "p(X). q(X) :- p(X). nat(z). nat(s(X)) :- nat(X). f(X1, X2) :- p(X1), q(X2). p(a). q(abc). f(abc, bc). g(suc(suc(X))) :- nat( s(X) ), f( abc, f(g(h(t))))."

rules :: [PG.E] -> PRs.Rules PRs.Identifier PRs.Identifier
rules str = PRs.rules $ fromJust $ PT.parse PG.prologGrammar (Just [' ', '\t', '\n']) PG.Start str

atom :: [PG.E] -> PRs.Atom PRs.Identifier PRs.Identifier
atom str = PRs.rhead $ head $ rules str

a = atom "p(g(X, V), X, a, V, s(T), T, h(c, d), c)."
b = atom "p(Y, s(Z), Z, s(W), W, b, h(c, d), c)."

main = do
    sequence_ $ map (putStrLn . show) (rules prog)
    print $ U.unify a b
    let t1 = T.insert (T.empty :: DT.DTrie Int Int) 1 1
    print $ T.find t1 1
    let t2 = T.insert t1 12 21
    let t3 = T.insert t2 13 31
    let t4 = T.insert t3 123 9
    print $ T.find t4 123
    print $ T.find t4 13
    print $ T.find t4 12
    print $ T.find t4 1
    print $ T.find t4 14
    print $ T.find t4 0
    print $ T.find t4 120
    let p1 = T.insert (T.empty :: ALT.AssocListTrie Char Int) "i" 1
    let p2 = T.insert p1 "iv" 2
    let p3 = T.insert p2 "ivo" 3
    let p4 = T.insert p3 "iv0" 0
    print $ T.find p1 "i"
    print $ T.find p3 "ivo"
    print $ T.find p3 "iv0"
    print $ T.find p4 "iv0"
    print $ T.find p1 "iva"