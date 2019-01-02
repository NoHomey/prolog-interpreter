module Main where

import qualified ParseTree as PT
import qualified PrologGrammar as PG
import qualified PrologRules as PRs
import qualified Unifier as U
import qualified DTrie as DT
import qualified KeyedCollection as KC
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

type C = ALT.AssocListTrie Char Int 

et :: C
et = KC.empty

main = do
    let rs = rules prog
    let mrs = PDB.transformRules (+1) (+1) (+1) (1, et) rs :: State ((Int, C), (Int, C)) (PRs.Rules Int Int Int)
    let mq = PDB.transformQuery (+1) (+1) (+1) [a] :: State ((Int, C), (Int, C), (Int, C)) (PRs.Atoms Int Int Int)
    let (db, (preds@(_, predsCol), syms@(_, symsCol))) = PDB.createDataBase (+1) (+1) (+1) ((1, et), (1, et)) (1, et) rs :: (DT.DTrie Int (PRs.Rules Int Int Int), ((Int, C), (Int, C))) 
    let q = evalState mq (preds, syms, (1, et))
    let r = R.resolve (+1) 0 db q
    print rs
    case fst r of
        Nothing -> print "No solution"
        Just u -> do
                    print u
                    --mapM_ print $ snd r
                    let r2 = R.next (+1) 0 db (snd r)
                    case fst r2 of
                        Nothing -> print "No solution"
                        Just u2 -> do
                                     print u2
                                     --mapM_ print $ snd r2
                                     let r3 = R.next (+1) 0 db (snd r2)
                                     --print $ snd r3
                                     case fst r3 of
                                         Nothing -> print "No solution"
                                         Just u3 -> do
                                                      print u3
                                                      let r4 = R.next (+1) 0 db (snd r3)
                                                      case fst r4 of
                                                          Nothing -> print "No solution"
                                                          Just u4 -> do
                                                                       print u4
                                                                       --print $ snd r4
                                                      
    