module Main where

import qualified ParseTree as PT
import qualified PrologGrammar as PG
import qualified PrologRules as PRs
import qualified Unifier as U
import qualified DTrie as DT
import qualified KeyedCollection as KC
import qualified AssocListTrie as ALT
import qualified PrologDataBase as PDB
import Control.Monad.State
import Data.Maybe

prog = "p(X). q(X) :- p(X). nat(z). nat(s(X)) :- nat(X). f(X1, X2) :- p(X1), q(X2). p(a). q(abc). f(abc, bc). g(suc(suc(X)), z) :- nat( s(X) ), f( abc, f(g(h(t))))."

rules :: [PG.E] -> PRs.Rules PRs.Identifier PRs.Identifier PRs.Identifier
rules str = PRs.rules $ fromJust $ PT.parse PG.prologGrammar (Just [' ', '\t', '\n']) PG.Start str

atom :: [PG.E] -> PRs.Atom PRs.Identifier PRs.Identifier PRs.Identifier
atom str = PRs.rhead $ head $ rules str

astr = "p(g(X, V), X, a, V, s(T), T, h(c, d), c)."
bstr = "p(Y, s(Z), Z, s(W), W, b, h(c, d), c)."
a = atom astr
b = atom bstr

type C = ALT.AssocListTrie Char Int 

et :: C
et = KC.empty

main = do
    let rs = rules prog
    let as = [a, b]
    let mrs = PDB.transformRules (+1) (+1) (+1) (1, et) rs :: State ((Int, C), (Int, C)) (PRs.Rules Int Int Int)
    let mas = PDB.transformAtoms (+1) (+1) (+1) as :: State ((Int, C), (Int, C), (Int, C)) (PRs.Atoms Int Int Int)
    let (db, ((_, preds), _)) = PDB.createDataBase (+1) (+1) (+1) ((1, et), (1, et)) (1, et) rs :: (DT.DTrie Int (PRs.Rules Int Int Int), ((Int, C), (Int, C))) 
    sequence_ $ map print rs
    sequence_ $ map print $ evalState mrs ((1, et), (1, et))
    sequence_ $ map print as
    sequence_ $ map print $ evalState mas ((1, et), (1, et), (1, et))
    let res = fromJust $ KC.find db $ fromJust $ KC.find preds "nat"
    print "first nat"
    print $ head res
    print "find nat"
    sequence_ $ map print $ res
    print $ U.compose [('y', PRs.Const 1), ('z', PRs.Const 2)] [('x', PRs.Func {PRs.funcSymbol = 3, PRs.params = [PRs.Var 'y']})]
    