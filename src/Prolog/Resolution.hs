module Prolog.Resolution (
    Query,
    Substitution,
    Rules,
    ResolutionPath,
    MaybeSubstitution,
    ResolutionResult,
    Next,
    KnowledgeBase,
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

type MaybeSubstitution s v = Maybe (Substitution s v)

type ResolutionResult p s v = (MaybeSubstitution s v, ResolutionPath p s v)

type ResolutionState p s v = State p s v (MaybeSubstitution s v)

type Next a = a -> a

type KnowledgeBase kb p s v = OU.KnowledgeBase kb p s v

step :: (Eq p, Eq s, Eq v, KC.KeyedCollection kb p)
     => Next v
     -> KnowledgeBase kb p s v
     -> ResolutionState p s v
     -> ResolutionPath p s v
     -> Rules p s v
     -> ResolutionState p s v
step nextRID kb retry p rs = firstSolution rs
    where h = head p
          g = goal h
          s = varsValues h
          rid = lastRID h
          target = U.substituteTerms s $ head g
          firstSolution []     = retry
          firstSolution (r:rs) = case U.unify target $ rename rid $ T.ruleHead r of
                                     Nothing -> firstSolution rs
                                     (Just s') -> let body = map (rename rid) $ T.body r
                                                      uh = ResolutionStep g s rs rid
                                                      nh = ResolutionStep (body ++ (tail g)) (U.compose s' s) [] $ nextRID rid
                                                  in (S.put $ nh:uh:(tail p)) >> (down nextRID kb)

backtrack :: (Eq p, Eq s, Eq v, KC.KeyedCollection kb p) => Next v -> KnowledgeBase kb p s v -> ResolutionState p s v
backtrack nextRID kb = do
                         p <- S.get
                         if null p
                           then return Nothing
                           else step nextRID kb ((S.put $ tail p) >> (backtrack nextRID kb)) p $ options $ head p

down :: (Eq p, Eq s, Eq v, KC.KeyedCollection kb p) => Next v -> KnowledgeBase kb p s v -> ResolutionState p s v
down nextRID kb = do
                    p <- S.get
                    let s = head p
                    let g = goal s
                    if null g
                      then return $ Just $ varsValues s
                      else let retry = backtrack nextRID kb
                           in maybe retry (step nextRID kb retry p) $ KC.find kb $ OU.identifier $ T.predSymbol $ head g

rename :: v -> OU.Atom p s v -> Atom p s v
rename rid = fmap ((,) rid)

result :: (Eq v) => v -> ResolutionState p s v -> ResolutionPath p s v -> ResolutionResult p s v
result rid st path = bimap (fmap (filter ((rid ==). fst . fst))) id $ S.runState st path

initialResolutionPath :: Next v -> v -> OU.Query p s v -> ResolutionPath p s v
initialResolutionPath nextRID rid q = [ResolutionStep (map (rename rid) q) U.empty [] $ nextRID rid]

resolve :: (Eq p, Eq s, Eq v, KC.KeyedCollection kb p)
        => Next v
        -> v
        -> KnowledgeBase kb p s v
        -> OU.Query p s v
        -> ResolutionResult p s v
resolve nextRID rid kb q = result rid (down nextRID kb) $ initialResolutionPath nextRID rid q

next :: (Eq p, Eq s, Eq v, KC.KeyedCollection kb p)
     => Next v
     -> v
     -> KnowledgeBase kb p s v
     -> ResolutionPath p s v
     -> ResolutionResult p s v
next nextRID rid kb path = result rid (backtrack nextRID kb) path