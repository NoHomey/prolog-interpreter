module Prolog.Resolution (
    Query,
    Substitution,
    Rules,
    ResolutionPath,
    MaybeSubstitution,
    ResolutionResult,
    DataBase,
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
                                           , options :: Maybe (Rules p s v)
                                           , lastRID :: v
                                           } deriving (Show)

type ResolutionPath p s v = [ResolutionStep p s v]

type State p s v a = S.State (ResolutionPath p s v) a

type MaybeSubstitution s v = Maybe (Substitution s v)

type ResolutionResult p s v = (MaybeSubstitution s v, ResolutionPath p s v)

type ResolutionState p s v = State p s v (MaybeSubstitution s v)

type Next a = a -> a

type DataBase p s v db = db (Rules p s v)

step :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => Next v -> DataBase p s v db -> ResolutionState p s v -> ResolutionPath p s v -> Maybe (Rules p s v) -> ResolutionState p s v
step nextRID db retry p rs = maybe retry firstThatUnifies rs
    where h = head p
          g = goal h
          s = varsValues h
          rid = lastRID h
          target = U.substituteTerms s $ head g
          firstThatUnifies []     = retry
          firstThatUnifies (r:rs) = case U.unify target $ rename rid $ T.ruleHead r of
                                        Nothing -> firstThatUnifies rs
                                        (Just s') -> do
                                                       let body = map (rename rid) $ T.body r
                                                       let uh = ResolutionStep g s (Just rs) rid
                                                       let nh = ResolutionStep (body ++ (tail g)) (U.compose s' s) Nothing $ nextRID rid
                                                       (S.put $ nh:uh:(tail p)) >> (down nextRID db)

backtrack :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => Next v -> DataBase p s v db -> ResolutionState p s v
backtrack nextRID db = do
                         p <- S.get
                         if null p
                           then return Nothing
                           else step nextRID db ((S.put $ tail p) >> (backtrack nextRID db)) p $ options $ head p

down :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => Next v -> DataBase p s v db -> ResolutionState p s v
down nextRID db = do
                    p <- S.get
                    let s = head p
                    let g = goal s
                    if null g
                      then return $ Just $ varsValues s
                      else step nextRID db (backtrack nextRID db) p $ KC.find db $ OU.identifier $ T.predSymbol $ head g

rename :: v -> OU.Atom p s v -> OU.Atom p s (v, v)
rename rid = fmap ((,) rid)

result :: (Eq v) => v -> ResolutionState p s v -> ResolutionPath p s v -> ResolutionResult p s v
result rid st path = bimap (fmap (filter ((rid ==). fst . fst))) id $ S.runState st path

initialResolutionPath :: Next v -> v -> OU.Query p s v -> ResolutionPath p s v
initialResolutionPath nextRID rid q = [ResolutionStep (map (rename rid) q) U.empty Nothing $ nextRID rid]

resolve :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => Next v -> v -> DataBase p s v db -> OU.Query p s v -> ResolutionResult p s v
resolve nextRID rid db q = result rid (down nextRID db) $ initialResolutionPath nextRID rid q

next :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => Next v -> v -> DataBase p s v db -> ResolutionPath p s v -> ResolutionResult p s v
next nextRID rid db path = result rid (backtrack nextRID db) path