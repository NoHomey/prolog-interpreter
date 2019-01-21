module Main where

import qualified Data.KeyedCollection as KC
import qualified Prolog.Parse.Grammar as G
import qualified Prolog.OptimizeForUnification as OU
import qualified Prolog.PipelineFromTextToOptimizedForUnification as PTOU
import qualified Prolog.Resolution as Res
import qualified Prolog.Unification as U
import qualified Prolog.Parse.Result as PR
import qualified Prolog.Types as T
import Data.AssocListTrie
import Data.DTrie
import Data.Either
import Data.Maybe
import Data.List
import Data.Bifunctor

type Identifier = Int

type NextIdentifier = Identifier -> Identifier

type RenameCollection = AssocListTrie G.Terminal Identifier

type RenameInfo = (Identifier, RenameCollection)

type Rules = OU.Rules Identifier Identifier Identifier

type DataBase = DTrie Identifier Rules

type QueryRenameInfo = (RenameInfo, RenameInfo, RenameInfo)

type Query = OU.Query Identifier Identifier Identifier

type Substitution = U.Substitution Identifier (Identifier, Identifier)

type ResolutionPath = Res.ResolutionPath Identifier Identifier Identifier

nextIdentifier :: NextIdentifier
nextIdentifier = (+1)

initialIdentifier :: Identifier
initialIdentifier = 1

initialRenameInfo :: RenameInfo
initialRenameInfo = (initialIdentifier, KC.empty)

pipelineForDB :: [G.Terminal] -> Either PTOU.ParseError (DataBase, (RenameInfo, RenameInfo))
pipelineForDB = PTOU.pipelineForDataBase nextIdentifier nextIdentifier nextIdentifier (initialRenameInfo , initialRenameInfo) initialRenameInfo 

pipelineForQuery :: QueryRenameInfo -> [G.Terminal] -> Either PTOU.ParseError (Query, QueryRenameInfo)
pipelineForQuery = PTOU.pipelineForQuery nextIdentifier nextIdentifier nextIdentifier

resolve :: DataBase -> Query -> (Maybe Substitution, ResolutionPath)
resolve = Res.resolve nextIdentifier initialIdentifier

unmapVar :: RenameCollection -> (Identifier, Identifier) -> PR.Identifier
unmapVar varsC = varToIdentifier $ KC.assoc varsC
    where varToIdentifier mapped v@(rid, x) = if rid == initialIdentifier
                                                then fst $ fromJust $ find ((x ==). snd) mapped
                                                else "$" ++ (show v) 

unmapSym :: RenameCollection -> OU.ArityIdentifier Identifier -> PR.Identifier
unmapSym symsC = symToIdentifier $ KC.assoc symsC
    where symToIdentifier mapped ai = fst $ fromJust $ find (((OU.identifier ai) ==). snd) mapped

showTermWithIdentifiers :: PR.Term -> String
showTermWithIdentifiers (T.Var x)     = x
showTermWithIdentifiers (T.Func f ps) = if null ps
                                         then f
                                         else f ++ "(" ++ (concat $ intersperse ", " $ map showTermWithIdentifiers ps) ++ ")" 

printSolution :: RenameCollection -> RenameCollection -> Substitution -> IO()
printSolution symsC varsC s = let idVar = unmapVar varsC
                                  idSym = unmapSym symsC
                                  idTerm = bimap idSym idVar
                                  identified = map (bimap idVar idTerm) s
                              in do
                                    print "Found solution."
                                    print "answers:"
                                    mapM_ (\(v, t) -> print $ v ++ " = " ++ (showTermWithIdentifiers t)) identified

printInfo :: (DataBase, Query, QueryRenameInfo) -> IO()
printInfo (db, q, ((p, predsC), (s, symsC), (v, varsC))) = case resolve db q of
                                                               (Nothing, _) -> print "No solution."
                                                               (Just sub, p) -> printSolution symsC varsC sub


content = "nat(zero). nat(X) :- nat(Y), is(X, succ(Y)). is(X, X)."

prompt = "nat(X)."

test :: Either PTOU.ParseError (DataBase, Query, QueryRenameInfo)
test = do
         (db, (preds, syms)) <- pipelineForDB content
         (query, info) <- pipelineForQuery (preds, syms, initialRenameInfo) prompt
         return (db, query, info)
                                
main = either print printInfo test