module Main where

import qualified ParseTree as PT
import qualified PrologGrammar as PG
import qualified PrologRules as PRs
import qualified Unifier as U
import qualified DTrie as DT
import qualified KeyedCollection as KC
import qualified AssocList as AL
import qualified AssocListTrie as ALT
import qualified PrologDataBase as PDB
import qualified Resolution as R
import Control.Monad.State
import Data.Maybe

--prog = "member2(X, l(X, l(X, T))). member2(X, l(Y, T)) :- member2(X, T)."
prog = "nat(z). nat(X) :- nat(Y), is(X, s(Y)). is(X, X)."

rules :: [PG.E] -> PRs.Rules PRs.Identifier PRs.Identifier PRs.Identifier
rules str = PRs.rules $ fromJust $ PT.parse PG.prologGrammar (Just [' ', '\t', '\n']) PG.Start str

atom :: [PG.E] -> PRs.Atom PRs.Identifier PRs.Identifier PRs.Identifier
atom str = PRs.rhead $ head $ rules str

--a = atom "member2(X, l(a, l(a, l(b, l(c, l(c, e))))))."
a = atom "nat(X)."

type C = ALT.AssocListTrie PG.E Int 
et :: C
et = KC.empty

type RulesWithIdentifiers = PRs.Rules PRs.Identifier PRs.Identifier PRs.Identifier

type QueryWithIdentifiers = R.Query PRs.Identifier PRs.Identifier PRs.Identifier

createDB :: RulesWithIdentifiers -> (DT.DTrie Int (PRs.Rules Int Int Int), ((Int, C), (Int, C)))
createDB = PDB.createDataBase (+1) (+1) (+1) ((1, et), (1, et)) (1, et)

evalQuery :: ((Int, C), (Int, C)) -> QueryWithIdentifiers -> (R.Query Int Int Int, ((Int, C), (Int, C), (Int, C))) 
evalQuery (preds, syms) q = runState (PDB.transformQuery (+1) (+1) (+1) q) (preds, syms, (1, et))

askForMore :: DT.DTrie Int (PRs.Rules Int Int Int) -> R.ResolutionPath Int Int Int -> IO ()
askForMore db p = do
                    print "Should I try to find more solutions? [y/n]"
                    l <- getLine
                    if null l
                      then askForMore db p
                      else let c = head l
                           in case c of
                                  'y' -> tryFindMore
                                  'Y' -> tryFindMore
                                  'n' -> return ()
                                  'N' -> return ()
                                  c -> askForMore db p
                          
    where tryFindMore = let (mu, np) = R.next (+1) 0 db p
                        in case mu of
                               Nothing -> print "No more solutions."
                               Just u -> do
                                           print u
                                           askForMore db np

queryMappedVarIdentifeirs :: QueryWithIdentifiers ->  C -> Int -> (Int, Int) -> PRs.Identifier
queryMappedVarIdentifeirs q varsCol id = let allVars = concatMap (concatMap U.vars . PRs.terms) q
                                             mapped = foldr insert KC.empty allVars
                                         in varToIdentifier mapped
        
    where insert :: PRs.Identifier -> AL.AssocList Int PRs.Identifier -> AL.AssocList Int PRs.Identifier
          insert varId mapped = let indx =  fromJust $ KC.find varsCol varId
                                    found = isJust $ KC.find mapped indx
                                in if found then mapped else KC.insert mapped indx varId
          
          varToIdentifier mapped v@(rid, x) = if rid == id
                                       then fromJust $ KC.find mapped x
                                       else "$" ++ (show v) 

main = do
    let rs = rules prog
    let (db, st) = createDB rs
    let (q, (_, (_, syms), (_, vars))) = evalQuery st [a]
    let (mu, p) = R.resolve (+1) 0 db q
    print rs
    case mu of
        Nothing -> print "No solution"
        Just u -> do
                    print u
                    askForMore db p