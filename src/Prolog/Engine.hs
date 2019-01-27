module Prolog.Engine (
    Solution,
    QueryPipeline,
    ParseErrorFrom(..),
    ParseError,
    EitherParseError,
    Text,
    engine
) where

import qualified Data.KeyedCollection as KC
import Prolog.Parse.Grammar (Terminal)
import Prolog.KnowledgeBase (Next, Nexts)
import qualified Prolog.OptimizeForUnification as OU
import Prolog.Pipeline (ParseErrorFrom(..), ParseError, EitherParseError, Text, pipeline)
import qualified Prolog.Resolution as Res
import Prolog.Parse.Result (Identifier, Term)
import Data.AssocListTrie
import Data.DTrie
import Data.Bifunctor
import Data.Maybe
import Data.List

type EngineID = Int

type RenameCollection = AssocListTrie Terminal EngineID

type RenameInfo = (EngineID, RenameCollection)

type Rules = OU.Rules EngineID EngineID EngineID

type KnowledgeBase = DTrie EngineID Rules

type QueryRenameInfo = (RenameInfo, RenameInfo, RenameInfo)

type RulesRenameInfo = (RenameInfo, RenameInfo)

type Query = OU.Query EngineID EngineID EngineID

type RenamedQueryInfo = (Query, QueryRenameInfo)

type ResolutionSubstitution = Res.Substitution EngineID EngineID EngineID

type Solution = [(Identifier, Term)]

type ResolutionRenamedVar = Res.RenamedVar EngineID EngineID

type PipelineResult = (KnowledgeBase, Text -> EitherParseError RenamedQueryInfo)

type QueryPipeline = Text -> EitherParseError [Solution]

nextEngineID :: Next EngineID
nextEngineID = (+1)

firstEngineID :: EngineID
firstEngineID = 1

emptyRenameCollection :: RenameCollection
emptyRenameCollection = KC.empty

emptyRenameInfo :: RenameInfo
emptyRenameInfo = (firstEngineID, emptyRenameCollection)

nextEngineIDs :: Nexts EngineID EngineID EngineID
nextEngineIDs = (nextEngineID, nextEngineID, nextEngineID)

rulesRenameInfo :: RulesRenameInfo
rulesRenameInfo = (emptyRenameInfo, emptyRenameInfo)

varsRenameInfo :: RenameInfo
varsRenameInfo = emptyRenameInfo

unmapVar :: RenameCollection -> ResolutionRenamedVar -> Identifier
unmapVar varsC = varToIdentifier $ KC.assoc varsC
    where varToIdentifier mapped v@(rid, x) = if rid == firstEngineID
                                                then fst $ fromJust $ find ((x ==). snd) mapped
                                                else "$" ++ (show v) 

unmapSym :: RenameCollection -> OU.ArityIdentifier EngineID -> Identifier
unmapSym symsC = symToIdentifier $ KC.assoc symsC
    where symToIdentifier mapped ai = fst $ fromJust $ find (((OU.identifier ai) ==). snd) mapped

solution :: RenameCollection -> RenameCollection -> ResolutionSubstitution -> Solution
solution symsC varsC s = let idVar = unmapVar varsC
                             idSym = unmapSym symsC
                             idTerm = bimap idSym idVar
                         in map (bimap idVar idTerm) s

createPipelineForQuery :: PipelineResult -> QueryPipeline
createPipelineForQuery (kb, action) text = fmap ignite $ action text
    where ignite (query, queryRenameInfo) = let liftSub = createSubLifter queryRenameInfo
                                                result = Res.resolve nextEngineID firstEngineID kb query
                                            in map liftSub result
          createSubLifter (_, (_, symsC), (_, varsC)) = solution symsC varsC
                                                            
engine :: Text -> EitherParseError QueryPipeline
engine text = fmap createPipelineForQuery $ pipeline nextEngineIDs rulesRenameInfo varsRenameInfo text