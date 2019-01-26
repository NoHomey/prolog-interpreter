{-# LANGUAGE ConstraintKinds #-}

module Prolog.Resolution (
    RenamedVar,
    Query,
    Substitution,
    Rules,
    ResolutionPath,
    ResolutionResult,
    Next,
    KnowledgeBase,
    Env,
    Backtrack,
    resolve,
    next
) where

import qualified Data.KeyedCollection as KC
import qualified Prolog.OptimizeForUnification as OU
import qualified Prolog.Unification as U
import qualified Prolog.Types as T
import qualified Control.Monad.State as S
import Data.Bifunctor
import Data.Maybe
import Data.Either
import Control.Monad

type RenamedVar r v = (r, v)

type Query r p s v = OU.Query p s (RenamedVar r v)

type Atom r p s v = OU.Atom p s (RenamedVar r v)

type Substitution r s v = U.Substitution s (RenamedVar r v)

type Rules p s v = OU.Rules p s v

data ResolutionStep r p s v = ResolutionStep { goal :: Query r p s v
                                           , varsValues :: Substitution r s v
                                           , options :: Rules p s v
                                           , lastRID :: r
                                           } deriving (Show)

type ResolutionPath r p s v = [ResolutionStep r p s v]

type State r p s v a = S.State (ResolutionPath r p s v) a

type Next a = a -> a

type KnowledgeBase kb p s v = OU.KnowledgeBase kb p s v

type Env kb r p s v = (Next r, r, KnowledgeBase kb p s v)

type Backtrack kb r p s v = (Env kb r p s v, ResolutionPath r p s v)

type ResolutionResult kb r p s v = Maybe (Substitution r s v, Backtrack kb r p s v)

type ResolutionState r p s v = State r p s v (Maybe (Substitution r s v))

type Requirement kb r p s v = (Eq r, Eq p, Eq s, Eq v, KC.KeyedCollection kb p)

rid :: RenamedVar r v -> r
rid = fst

step :: Requirement kb r p s v
     => Env kb r p s v
     -> ResolutionState r p s v
     -> ResolutionPath r p s v
     -> Rules p s v
     -> ResolutionState r p s v
step env@(nextRID, qRID, kb) retry p rs = firstSolution rs
    where h = head p
          g = goal h
          s = varsValues h
          last = lastRID h
          firstSolution []     = retry
          firstSolution (r:rs) = case U.unify (head g) $ rename last $ T.ruleHead r of
                                     Nothing -> firstSolution rs
                                     (Just s') -> let body = map (rename last) $ T.body r
                                                      uh = ResolutionStep g s rs last
                                                      ng = map (U.substituteTerms s') $ body ++ (tail g)
                                                      ns = trackValues $ U.compose s' s
                                                      nh = ResolutionStep ng ns [] $ nextRID last
                                                  in (S.put $ nh:uh:tail p) >> (down env)
          trackValues s = let vars = filter ((qRID ==). rid . fst) s
                              toVar = [(y, T.Var x) | (x, t) <- s, let y = varIdentifier t, T.isVar t, qRID == (rid y)]
                          in vars ++ toVar
          varIdentifier v = fromRight undefined $ T.identifier v

backtrack :: Requirement kb r p s v => Env kb r p s v -> ResolutionState r p s v
backtrack env = do
                  p <- S.get
                  if null p
                    then return Nothing
                    else let retry = (S.put $ tail p) >> (backtrack env)
                         in step env retry p $ options $ head p

down :: Requirement kb r p s v => Env kb r p s v -> ResolutionState r p s v
down env@(_, _, kb) = do
                        p <- S.get
                        let s = head p
                        let g = goal s
                        if null g
                          then return $ Just $ varsValues s
                          else let retry = backtrack env
                               in maybe retry (step env retry p) $ findOptions $ head g
    where findOptions a = KC.find kb $ OU.identifier $ T.predSymbol a

rename :: r -> OU.Atom p s v -> Atom r p s v
rename rid = fmap ((,) rid)

initialResolutionPath :: Next r -> r -> OU.Query p s v -> ResolutionPath r p s v
initialResolutionPath nextRID qRID q = [ResolutionStep (map (rename qRID) q) U.empty [] $ nextRID qRID]

result :: Requirement kb r p s v
       => Env kb r p s v 
       -> ResolutionState r p s v
       -> ResolutionPath r p s v
       -> ResolutionResult kb r p s v
result env m st = let (ms, p) = S.runState m st
                  in fmap (\s -> (s, (env, p))) ms

resolve :: Requirement kb r p s v
        => Next r
        -> r
        -> KnowledgeBase kb p s v
        -> OU.Query p s v
        -> ResolutionResult kb r p s v
resolve nextRID qRID kb q = let env = (nextRID, qRID, kb)
                                m = down env
                                st = initialResolutionPath nextRID qRID q
                            in result env m st

next :: Requirement kb r p s v => Backtrack kb r p s v -> ResolutionResult kb r p s v
next (env, path) =  result env (backtrack env) path