module Prolog.Resolution (
    ResolutionPath,
    resolve,
    next
) where

import qualified Data.KeyedCollection as KC
import qualified Prolog.OptimizeForUnification as OU
import qualified Prolog.Unification as U
import qualified Prolog.Types as T
import qualified Control.Monad.State as S

data ResolutionStep p s v = ResolutionStep {goal :: OU.Query p s (v, v), varsValues :: U.Substitution s (v, v), options :: OU.Rules p s v, rid :: v} deriving (Show)

type ResolutionPath p s v = [ResolutionStep p s v]

moveUp :: S.State (ResolutionPath p s v) Bool
moveUp = S.state $ \(_:p) -> tryToMove p
    where tryToMove [] = (False, [])
          tryToMove p@(l:rs) = if null $ options l then tryToMove rs else (True, p)

continue :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => (v -> v) -> db (OU.Rules p s v) -> S.State (ResolutionPath p s v) (Maybe (U.Substitution s (v, v)))
continue nextId db = do
                       c <- S.get
                       let p = tail c
                       S.put $ p
                       process nextId db (head p) (options $ head c)

restart :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => (v -> v) -> db (OU.Rules p s v) -> S.State (ResolutionPath p s v) (Maybe (U.Substitution s (v, v)))
restart nextId db = do
                       c <- S.get
                       if null $ options $ head c
                          then do
                                 S.put $ tail c
                                 restart nextId db
                          else continue nextId db

backtrack :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => (v -> v) -> db (OU.Rules p s v) -> S.State (ResolutionPath p s v) (Maybe (U.Substitution s (v, v)))
backtrack nextId db = do
                        moved <- moveUp
                        if moved then continue nextId db else return Nothing

process :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => (v -> v) -> db (OU.Rules p s v) -> ResolutionStep p s v -> OU.Rules p s v -> S.State (ResolutionPath p s v) (Maybe (U.Substitution s (v, v)))
process nextId db l rules = tryMatch rules
    where g = head $ goal l
          vals = varsValues l
          id = rid l
          g' = subs vals g
          tryMatch [] = backtrack nextId db
          tryMatch (r:rs) = case U.unify g' (rename id (T.ruleHead r)) of
                                Nothing -> tryMatch rs
                                Just u -> do
                                            p <- S.get
                                            let u' = U.compose u vals
                                            S.put $ (ResolutionStep (addBody u' r) u' rs (nextId id)):p
                                            resolutionStep nextId db
          addBody u rule = (map (subs u . rename id) (T.body rule)) ++ (tail $ goal l)
          subs u (T.Atom p ts) = T.Atom p $ map (U.substitute $ U.substitutionFuncFromSubstitution u) ts

resolutionStep :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => (v -> v) -> db (OU.Rules p s v) -> S.State (ResolutionPath p s v) (Maybe (U.Substitution s (v, v)))
resolutionStep nextId db = do
                             p <- S.get
                             let l = head p
                             if null $ goal l
                               then return $ Just $ varsValues l
                               else case KC.find db $ OU.identifier $ T.predSymbol $ head $ goal l of
                                        Nothing -> backtrack nextId db
                                        Just rules -> process nextId db l rules

rename :: v -> OU.Atom p s v -> OU.Atom p s (v, v)
rename id (T.Atom p ts) = T.Atom p $ map (fmap ((,) id)) ts

transform :: (Eq v) => v -> S.State (ResolutionPath p s v) (Maybe (U.Substitution s (v, v))) -> ResolutionPath p s v -> (Maybe (U.Substitution s (v, v)), ResolutionPath p s v)
transform id st path = let (u, p) = S.runState st path
                       in (fmap (filter ((id == ). fst . fst)) u, p) 

resolve :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => (v -> v) -> v -> db (OU.Rules p s v) -> OU.Query p s v -> (Maybe (U.Substitution s (v, v)), ResolutionPath p s v) 
resolve nextId id db q = transform id (resolutionStep nextId db) [ResolutionStep (map (rename id) q) U.empty [] (nextId id)]

next :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => (v -> v) -> v -> db (OU.Rules p s v) -> ResolutionPath p s v -> (Maybe (U.Substitution s (v, v)), ResolutionPath p s v)
next nextId id db path = transform id (restart nextId db) path