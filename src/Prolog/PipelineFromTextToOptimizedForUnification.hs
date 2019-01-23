{-# LANGUAGE FlexibleContexts #-}

module Prolog.PipelineFromTextToOptimizedForUnification (
    ParseErrorFrom(..),
    ParseError,
    RenamedQueryInfo,
    DataBaseInfo,
    pipelineForDataBase,
    pipelineForQuery
) where

import qualified Prolog.Parse.ParseTree as PT
import qualified Prolog.Parse.Result as PR
import qualified Prolog.Parse.Grammar as PG
import qualified Prolog.DataBase as DB
import qualified Prolog.OptimizeForUnification as OU
import qualified Data.KeyedCollection as KC
import Data.Bifunctor

data ParseErrorFrom = Rules | Query deriving (Eq, Show)

type ParseError = (ParseErrorFrom, PT.Counter)

type RenamedQueryInfo p predsC s symsC v varsC = (OU.Query p s v, DB.AtomRenameInfo p predsC s symsC v varsC)

type DataBaseInfo db p predsC s symsC v = (OU.DataBase db p s v, DB.RuleRenameInfo p predsC s symsC)

mapErrors :: ParseErrorFrom -> [PT.Counter] -> ParseError
mapErrors from cs = (from, maximum cs)

mapParseTreeForQuery ::
                     ( KC.KeyedCollection predsC PR.Identifier
                     , KC.KeyedCollection symsC PR.Identifier
                     , KC.KeyedCollection varsC PR.Identifier
                     )
                     => DB.Next p
                     -> DB.Next s
                     -> DB.Next v
                     -> DB.AtomRenameInfo p predsC s symsC v varsC
                     -> PT.ParseTree
                     -> RenamedQueryInfo p predsC s symsC v varsC
mapParseTreeForQuery np ns nv st pt = let map = bimap OU.optimizeQueryForUnification id
                                          renameQuery = DB.renameQuery np ns nv st
                                      in map $ renameQuery $ PR.query pt

mapParseTreeForRules ::
                     ( Eq p
                     , KC.KeyedCollection predsC PR.Identifier
                     , KC.KeyedCollection symsC PR.Identifier
                     , KC.KeyedCollection varsC PR.Identifier
                     , KC.KeyedCollection rulesC p
                     , Functor rulesC
                     )
                     => DB.Next p
                     -> DB.Next s
                     -> DB.Next v
                     -> DB.RuleRenameInfo p predsC s symsC
                     -> DB.RenameInfo v varsC
                     -> PT.ParseTree
                     -> DataBaseInfo rulesC p predsC s symsC v
mapParseTreeForRules np ns nv st v pt = let map = bimap OU.optimizeDataBaseForUnification id
                                            createDB = DB.createDataBase np ns nv st v
                                        in map $ createDB $ PR.rules pt

pipelineForQuery ::
                 ( KC.KeyedCollection predsC PR.Identifier
                 , KC.KeyedCollection symsC PR.Identifier
                 , KC.KeyedCollection varsC PR.Identifier
                 )
                 => DB.Next p
                 -> DB.Next s
                 -> DB.Next v
                 -> DB.AtomRenameInfo p predsC s symsC v varsC
                 -> [PG.Terminal]
                 -> Either ParseError (RenamedQueryInfo p predsC s symsC v varsC)
pipelineForQuery np ns nv st text = let mapParseErrors = mapErrors Query
                                        mapParseTrees = (mapParseTreeForQuery np ns nv st) . head
                                        parseResult = PT.parse PG.Query text
                                    in bimap mapParseErrors mapParseTrees parseResult
                                    
pipelineForDataBase ::
                    ( Eq p
                    , KC.KeyedCollection predsC PR.Identifier
                    , KC.KeyedCollection symsC PR.Identifier
                    , KC.KeyedCollection varsC PR.Identifier
                    , KC.KeyedCollection rulesC p
                    , Functor rulesC
                    )
                    => DB.Next p
                    -> DB.Next s
                    -> DB.Next v
                    -> DB.RuleRenameInfo p predsC s symsC
                    -> DB.RenameInfo v varsC
                    -> [PG.Terminal]
                    -> Either ParseError (DataBaseInfo rulesC p predsC s symsC v)
pipelineForDataBase np ns nv st v text = let mapParseErrors = mapErrors Rules
                                             mapParseTrees = (mapParseTreeForRules np ns nv st v) . head
                                             parseResult = PT.parse PG.Start text
                                         in bimap mapParseErrors mapParseTrees parseResult