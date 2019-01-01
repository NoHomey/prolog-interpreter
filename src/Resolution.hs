module Resolution (
    Query,
    ResolutionPath,
    resolve,
    next
) where

import qualified KeyedCollection as KC
import qualified PrologRules as PRs
import qualified Unifier as U
import Control.Monad.State

type Query p s v = PRs.Atoms p s v

data ResolutionStep p s v = ResolutionStep {goal :: Query p s (v, v), varsValues :: U.Unifier s (v, v), options :: PRs.Rules p s v, rid :: v} deriving (Show)

type ResolutionPath p s v = [ResolutionStep p s v]

moveUp :: State (ResolutionPath p s v) Bool
moveUp = state $ \(_:p) -> tryToMove p
    where tryToMove [] = (False, [])
          tryToMove p@(l:rs) = if null $ options l then tryToMove rs else (True, p)

continue :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => (v -> v) -> db (PRs.Rules p s v) -> State (ResolutionPath p s v) (Maybe (U.Unifier s (v, v)))
continue nextId db = do
                       (c:p@(l:_)) <- get
                       put p
                       process nextId (rid c) db l (options c)

backtrack :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => (v -> v) -> db (PRs.Rules p s v) -> State (ResolutionPath p s v) (Maybe (U.Unifier s (v, v)))
backtrack nextId db = do
                        moved <- moveUp
                        if moved then continue nextId db else return Nothing

process :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => (v -> v) -> v -> db (PRs.Rules p s v) -> ResolutionStep p s v -> PRs.Rules p s v -> State (ResolutionPath p s v) (Maybe (U.Unifier s (v, v)))
process nextId id db l rules = tryMatch rules
    where g = head $ goal l
          vals = varsValues l
          g' = PRs.Atom {PRs.predSymbol = PRs.predSymbol g, PRs.terms = map (U.applySubstitution (U.substitution vals)) $ PRs.terms g}
          tryMatch [] = backtrack nextId db
          tryMatch (r:rs) = case U.unify g (rename id (PRs.rhead r)) of
                                Nothing -> tryMatch rs
                                Just u -> do
                                            p <- get
                                            put $ (step (addBody u r) (U.compose u vals) rs (nextId id)):p
                                            resolutionStep nextId db
          addBody u rule = (map (subs u . rename id) (PRs.body rule)) ++ (tail $ goal l)
          subs u a = PRs.Atom {PRs.predSymbol = PRs.predSymbol a, PRs.terms = map (U.applySubstitution (U.substitution u)) $ PRs.terms a}    

resolutionStep :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => (v -> v) -> db (PRs.Rules p s v) -> State (ResolutionPath p s v) (Maybe (U.Unifier s (v, v)))
resolutionStep nextId db = do
                             p@(l:rs) <- get
                             if null $ goal l
                               then return $ Just $ varsValues l
                               else case KC.find db $ PRs.predSymbol $ head $ goal l of
                                        Nothing -> backtrack nextId db
                                        Just rules -> process nextId (rid l) db l rules

step :: Query p s (v, v) -> U.Unifier s (v, v) -> PRs.Rules p s v -> v -> ResolutionStep p s v
step q u rs id = ResolutionStep {goal = q, varsValues = u, options = rs, rid = id}

rename :: v -> PRs.Atom p s v -> PRs.Atom p s (v, v)
rename id a = PRs.Atom {PRs.predSymbol = PRs.predSymbol a, PRs.terms = map (fmap (\x -> (id, x))) (PRs.terms a)}

transform :: (Eq v) => v -> State (ResolutionPath p s v) (Maybe (U.Unifier s (v, v))) -> ResolutionPath p s v -> (Maybe [(v, PRs.Term s (v, v))], ResolutionPath p s v)
transform id st path = let (u, p) = runState st path
                       in (fmap (map (\((_, x), t) -> (x, t)) . filter ((id == ). fst . fst)) u, p) 

resolve :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => (v -> v) -> v -> db (PRs.Rules p s v) -> Query p s v -> (Maybe [(v, PRs.Term s (v, v))], ResolutionPath p s v) 
resolve nextId id db q = transform id (resolutionStep nextId db) [step (map (rename id) q) U.empty [] id]

next :: (Eq p, Eq s, Eq v, KC.KeyedCollection db p) => (v -> v) -> v -> db (PRs.Rules p s v) -> ResolutionPath p s v -> (Maybe [(v, PRs.Term s (v, v))], ResolutionPath p s v)
next nextId id db path = transform id (continue nextId db) path
