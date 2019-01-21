module Main where

import qualified Data.KeyedCollection as KC
import qualified Prolog.Parse.Grammar as G
import qualified Prolog.OptimizeForUnification as OU
import qualified Prolog.PipelineFromTextToOptimizedForUnification as PTOU
import Data.AssocListTrie
import Data.DTrie
import Data.Either

type Identifier = Int

type NextIdentifier = Identifier -> Identifier

type RenameCollection = AssocListTrie G.Terminal Identifier

type RenameInfo = (Identifier, RenameCollection)

type DataBase = DTrie Identifier (OU.Rules Identifier Identifier Identifier)

type QueryRenameInfo = (RenameInfo, RenameInfo, RenameInfo)

type Query = OU.Query Identifier Identifier Identifier

nextIdentifier :: NextIdentifier
nextIdentifier = (+1)

initialRenameInfo :: RenameInfo
initialRenameInfo = (1, KC.empty)

pipelineForDB :: [G.Terminal] -> Either PTOU.ParseError (DataBase, (RenameInfo, RenameInfo))
pipelineForDB = PTOU.pipelineForDataBase nextIdentifier nextIdentifier nextIdentifier (initialRenameInfo , initialRenameInfo) initialRenameInfo 

pipelineForQuery :: QueryRenameInfo -> [G.Terminal] -> Either PTOU.ParseError (Query, QueryRenameInfo)
pipelineForQuery = PTOU.pipelineForQuery nextIdentifier nextIdentifier nextIdentifier

printDB :: DataBase -> IO()
printDB db = mapM_ printPredInfo $ KC.assoc db
    where printPredInfo (p, rs) = do
                                    print "pred:"
                                    print  p
                                    print "rules:"
                                    mapM print rs

printQuery :: Query -> IO()
printQuery query = do
                     print "query:"
                     mapM_ print $ query

printInfo :: (DataBase, Query, QueryRenameInfo) -> IO()
printInfo (db, q, _) = do
                         printDB db
                         printQuery q

content = "nat(zero). nat(X) :- nat(Y), is(X, succ(Y)). is(X, X)."

prompt = "nat(Y), is(X, succ(Y))."

test :: Either PTOU.ParseError (DataBase, Query, QueryRenameInfo)
test = do
         (db, (preds, syms)) <- pipelineForDB content
         (query, info) <- pipelineForQuery (preds, syms, initialRenameInfo) prompt
         return (db, query, info)
                                
main = either print printInfo test