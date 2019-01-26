{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Prolog.Pipeline (
    ParseErrorFrom(..),
    ParseError,
    RenamedQueryInfo,
    EitherParseError,
    Text,
    PipelineForQuery,
    pipeline
) where

import qualified Prolog.Parse.ParseTree as PT
import qualified Prolog.Parse.Result as PR
import qualified Prolog.Parse.Grammar as PG
import qualified Prolog.KnowledgeBase as KB
import qualified Prolog.OptimizeForUnification as OU
import qualified Data.KeyedCollection as KC
import Data.Bifunctor

data ParseErrorFrom = Rules | Query deriving (Eq, Show)

type ParseError = (ParseErrorFrom, PT.Counter)

type RenamedQueryInfo p predsC s symsC v varsC = (OU.Query p s v, KB.AtomRenameInfo p predsC s symsC v varsC)

type EitherParseError a = Either ParseError a

type Text = PT.Text

type PipelineForQuery p predsC s symsC v varsC  = Text -> EitherParseError (RenamedQueryInfo p predsC s symsC v varsC)

type PipelineResult kb p predsC s symsC v varsC  = (OU.KnowledgeBase kb p s v, PipelineForQuery p predsC s symsC v varsC)

type IdentifierKeyedCollectionConstraint c = KC.KeyedCollection c PR.Identifier

type RenamesConstraint predsC symsC varsC = ( IdentifierKeyedCollectionConstraint predsC
                                            , IdentifierKeyedCollectionConstraint symsC
                                            , IdentifierKeyedCollectionConstraint varsC
                                            )

type PipelineConstraint predsC symsC varsC rulesC p = (RenamesConstraint predsC symsC varsC, KB.KnowledgeBaseConstraint rulesC p, Eq p)

mapErrors :: ParseErrorFrom -> [PT.Counter] -> ParseError
mapErrors from cs = (from, maximum cs)

pipeline :: PipelineConstraint predsC symsC varsC rulesC p
         => KB.Nexts p s v
         -> KB.RuleRenameInfo p predsC s symsC
         -> KB.RenameInfo v varsC
         -> Text
         -> EitherParseError (PipelineResult rulesC p predsC s symsC v varsC)
pipeline nexts ruleRenameInfo varsRenameInfo text = let optimize = OU.optimizeKnowledgeBaseForUnification
                                                        rename = KB.createKnowledgeBase nexts ruleRenameInfo varsRenameInfo
                                                        consume = PR.rules
                                                        pipelineForKnowledgeBase = result optimize rename consume Rules PG.KnowledgeBase
                                                    in fmap (second createPipelineForQuery) $ pipelineForKnowledgeBase text
    where mapParseTree optimize rename consume pt = first optimize $ rename $ consume pt
          result optimize rename consume errorFrom start text = let mapParseErrors = mapErrors errorFrom
                                                                    mapParseTrees = (mapParseTree optimize rename consume) . head
                                                                    parseResult = PT.parse start text
                                                                in bimap mapParseErrors mapParseTrees parseResult
          createPipelineForQuery (predsInfo, symsInfo) = let optimize = OU.optimizeQueryForUnification
                                                             rename = KB.renameQuery nexts (predsInfo, symsInfo, varsRenameInfo)
                                                             consume = PR.query
                                                         in result optimize rename consume Query PG.Query