module Main where

import qualified ParseTree as PT
import qualified PrologGrammar as PG
import qualified PrologRules as PRs
import qualified Unifier as U
import qualified PrologDataBase as PDB
import qualified Resolution as R
import qualified Data.DTrie as DT
import qualified Data.KeyedCollection as KC
import qualified Data.AssocListTrie as ALT
import Control.Monad.State
import Data.Maybe
import Data.List

--prog = "member2(X, l(X, l(X, T))). member2(X, l(Y, T)) :- member2(X, T)."
--prog = "nat(zero). nat(X) :- nat(Y), is(X, succ(Y)). is(X, X)."
prog = "p(david, jhon). p(jim, david). p(steve, jim). p(elvis, steve). a(A, B) :- p(A, B). a(A, B) :- p(A, X), a(X, B)."

rules :: [PG.E] -> PRs.Rules PRs.Identifier PRs.Identifier PRs.Identifier
rules str = PRs.rules $ fromJust $ PT.parse PG.prologGrammar (Just [' ', '\t', '\n']) PG.Start str

atom :: [PG.E] -> PRs.Atom PRs.Identifier PRs.Identifier PRs.Identifier
atom str = PRs.rhead $ head $ rules str

--a = atom "member2(X, l(a, l(a, l(b, l(c, l(c, e))))))."
--a = atom "nat(X)."
a = atom "a(elvis, X)."

type C = ALT.AssocListTrie PG.E Int 
et :: C
et = KC.empty

type RulesWithIdentifiers = PRs.Rules PRs.Identifier PRs.Identifier PRs.Identifier

type QueryWithIdentifiers = R.Query PRs.Identifier PRs.Identifier PRs.Identifier

createDB :: RulesWithIdentifiers -> (DT.DTrie Int (PRs.Rules Int Int Int), ((Int, C), (Int, C)))
createDB = PDB.createDataBase (+1) (+1) (+1) ((1, et), (1, et)) (1, et)

evalQuery :: ((Int, C), (Int, C)) -> QueryWithIdentifiers -> (R.Query Int Int Int, ((Int, C), (Int, C), (Int, C))) 
evalQuery (preds, syms) q = runState (PDB.transformQuery (+1) (+1) (+1) q) (preds, syms, (1, et))

askForMore :: DT.DTrie Int (PRs.Rules Int Int Int) -> R.ResolutionPath Int Int Int -> (U.Unifier Int (Int, Int) -> IO ()) -> IO ()
askForMore db p printSol = do
                             print "Should I try to find more solutions? [y/n]"
                             l <- getLine
                             if null l
                               then askForMore db p printSol
                               else let c = head l
                                    in case c of
                                           'y' -> tryFindMore
                                           'Y' -> tryFindMore
                                           'n' -> return ()
                                           'N' -> return ()
                                           c -> askForMore db p printSol
                          
    where tryFindMore = let (mu, np) = R.next (+1) 0 db p
                        in case mu of
                               Nothing -> print "No more solutions."
                               Just u -> do
                                           printSol u
                                           askForMore db np printSol

unmapVar :: C -> Int -> (Int, Int) -> PRs.Identifier
unmapVar varsCol id = varToIdentifier (KC.assoc varsCol)
    where varToIdentifier mapped v@(rid, x) = if rid == id
                                                then fst $ fromJust $ find ((x ==). snd) mapped
                                                else "$" ++ (show v) 

unmapSym :: C -> Int ->  PRs.Identifier
unmapSym symsCol = symToIdentifier (KC.assoc symsCol)
    where symToIdentifier mapped s = fst $ fromJust $ find ((s ==). snd) mapped

showTermWithIdentifiers :: PRs.Term PRs.Identifier PRs.Identifier -> String
showTermWithIdentifiers t = case t of
                                (PRs.Var x) -> x
                                (PRs.Const c) -> c
                                (PRs.Func f ps) -> f ++ "(" ++ (concat $ intersperse ", " $ map showTermWithIdentifiers ps) ++ ")"

printSolution :: C -> Int -> C -> U.Unifier Int (Int, Int) -> IO ()
printSolution varsCol id symsCol u = let uVar = unmapVar varsCol id
                                         uSym = unmapSym symsCol
                                         unmapSyms = PRs.mapTerm uSym uVar
                                         identified = [(uVar v, unmapSyms t) | (v, t) <- u]
                                     in do
                                          print "answers:"
                                          mapM_ (\(v, t) -> print $ v ++ " = " ++ (showTermWithIdentifiers t)) identified
                                          return ()

main = do
    let rs = rules prog
    let (db, st) = createDB rs
    let (q, (_, (_, syms), (_, vars))) = evalQuery st [a]
    let (mu, p) = R.resolve (+1) 0 db q
    let printSol = printSolution vars 0 syms
    print rs
    case mu of
        Nothing -> print "No solution"
        Just u -> do
                    printSol u
                    askForMore db p printSol