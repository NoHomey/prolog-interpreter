module Prolog.Resolution (
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

type Query p s v = OU.Query p s (v, v)

type Atom p s v = OU.Atom p s (v, v)

type Substitution s v = U.Substitution s (v, v)

type Rules p s v = OU.Rules p s v

data ResolutionStep p s v = ResolutionStep { goal :: Query p s v
                                           , varsValues :: Substitution s v
                                           , options :: Rules p s v
                                           , lastRID :: v
                                           } deriving (Show)

type ResolutionPath p s v = [ResolutionStep p s v]

type State p s v a = S.State (ResolutionPath p s v) a

type Env kb p s v = (Next v, v, KnowledgeBase kb p s v)

type Backtrack kb p s v = (Env kb p s v, ResolutionPath p s v)

type ResolutionResult kb p s v = Maybe (Substitution s v, Backtrack kb p s v)

type ResolutionState p s v = State p s v (Maybe (Substitution s v))

type Next a = a -> a

type KnowledgeBase kb p s v = OU.KnowledgeBase kb p s v

rid :: (v, v) -> v
rid = fst

step :: (Eq p, Eq s, Eq v, KC.KeyedCollection kb p)
     => Env kb p s v
     -> ResolutionState p s v
     -> ResolutionPath p s v
     -> Rules p s v
     -> ResolutionState p s v
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

backtrack :: (Eq p, Eq s, Eq v, KC.KeyedCollection kb p) => Env kb p s v -> ResolutionState p s v
backtrack env = do
                  p <- S.get
                  if null p
                    then return Nothing
                    else let retry = (S.put $ tail p) >> (backtrack env)
                          in step env retry p $ options $ head p

down :: (Eq p, Eq s, Eq v, KC.KeyedCollection kb p) => Env kb p s v -> ResolutionState p s v
down env@(_, _, kb) = do
                        p <- S.get
                        let s = head p
                        let g = goal s
                        if null g
                          then return $ Just $ varsValues s
                          else let retry = backtrack env
                               in maybe retry (step env retry p) $ findOptions $ head g
    where findOptions a = KC.find kb $ OU.identifier $ T.predSymbol a

rename :: v -> OU.Atom p s v -> Atom p s v
rename qRID = fmap ((,) qRID)

initialResolutionPath :: Next v -> v -> OU.Query p s v -> ResolutionPath p s v
initialResolutionPath nextRID qRID q = [ResolutionStep (map (rename qRID) q) U.empty [] $ nextRID qRID]

result :: (Eq p, Eq s, Eq v, KC.KeyedCollection kb p)
       => Env kb p s v 
       -> ResolutionState p s v
       -> ResolutionPath p s v
       -> ResolutionResult kb p s v
result env m st = let (ms, p) = S.runState m st
                  in fmap (\s -> (s, (env, p))) ms

resolve :: (Eq p, Eq s, Eq v, KC.KeyedCollection kb p)
        => Next v
        -> v
        -> KnowledgeBase kb p s v
        -> OU.Query p s v
        -> ResolutionResult kb p s v
resolve nextRID qRID kb q = let env = (nextRID, qRID, kb)
                                m = down env
                                st = initialResolutionPath nextRID qRID q
                            in result env m st

next :: (Eq p, Eq s, Eq v, KC.KeyedCollection kb p) => Backtrack kb p s v -> ResolutionResult kb p s v
next (env, path) =  result env (backtrack env) path