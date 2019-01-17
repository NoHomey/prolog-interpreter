--{-# LANGUAGE AllowAmbiguousTypes #-}

module PrologDataBase (
    TransformState,
    mapTermM,
    mapAtomM,
    mapRuleM,
    transformPreds,
    transformSyms,
    transformVars,
    transformRule,
    transformRules,
    transformAtom,
    transformAtoms,
    transformQuery,
    createDataBase
) where

import PrologRules
import qualified Data.KeyedCollection as KC
import Data.KeyToPath
import Control.Monad.State

type TransformState predsC symsC varsC a b c d = State ((a, predsC a), (b, symsC b), (c, varsC c)) d 

mapTermM :: Monad m => (a -> m c) -> (b -> m d) -> Term a b -> m (Term c d)
mapTermM f g t = case t of
                       (Var x) -> g x >>= (\x' -> return $ Var x')
                       (Const c) -> f c >>= (\c' -> return $ Const c')
                       (Func s ps) -> do
                                        s' <- f s
                                        ps' <- mapM (mapTermM f g) ps
                                        return $ Func {funcSymbol = s', params = ps'}

mapAtomM :: Monad m => (a -> m c) -> (b -> m d) -> (x -> m y) -> Atom a b x -> m (Atom c d y)
mapAtomM h f g a = do
                      p <- h $ predSymbol a
                      ts <- mapM (mapTermM f g) $ terms a
                      return $ Atom {predSymbol = p, terms = ts}

mapRuleM :: Monad m => (a -> m c) ->  (b -> m d) -> (x -> m y) -> Rule a b x -> m (Rule c d y)
mapRuleM h f g r = do
                     let m = mapAtomM h f g
                     h <- m $ rhead r
                     bs <- mapM m $ body r
                     return $ Rule {rhead = h, body = bs}

transform :: (Eq a, KC.KeyedCollection col a) => (b -> b) -> (b, col b) -> a -> (b, (b, col b))
transform update (st, col) k = case KC.find col k of
                                   Nothing -> let col' = KC.insert col k st
                                                  st' = update st
                                              in (st, (st', col'))
                                   Just v -> (v, (st, col))

transformPreds :: (Eq a, KC.KeyedCollection predsC a) => (b -> b) -> a -> TransformState predsC symsC varsC b d y b
transformPreds update pred = state action
    where action (preds, syms, vars) = let (pred', preds') = transform update preds pred 
                                       in (pred', (preds', syms, vars))

transformSyms :: (Eq c, KC.KeyedCollection symsC c) => (d -> d) -> c -> TransformState predsC symsC varsC b d y d
transformSyms update sym = state action
    where action (preds, syms, vars) = let (sym', syms') = transform update syms sym 
                                       in (sym', (preds, syms', vars))
                            
transformVars :: (Eq x, KC.KeyedCollection varsC x) => (y -> y) -> x -> TransformState predsC symsC varsC b d y y
transformVars update var = state action
    where action (preds, syms, vars) = let (var', vars') = transform update vars var 
                                       in (var', (preds, syms, vars'))

transformRule :: (Eq a, Eq c, Eq x, KC.KeyedCollection predsC a, KC.KeyedCollection symsC c, KC.KeyedCollection varsC x) =>
                 (b -> b) -> (d -> d) -> (y -> y) -> (y, varsC y) -> Rule a c x -> State ((b, predsC b), (d, symsC d)) (Rule b d y)
transformRule nextPred nextSym nextVar vars rule = state action
    where action (preds, syms) = let m = mapRuleM (transformPreds nextPred) (transformSyms nextSym) (transformVars nextVar) rule
                                     (rule', (preds', syms', _)) = runState m (preds, syms, vars)
                                 in (rule', (preds', syms'))

transformRules :: (Eq a, Eq c, Eq x, KC.KeyedCollection predsC a, KC.KeyedCollection symsC c, KC.KeyedCollection varsC x) =>
                  (b -> b) -> (d -> d) -> (y -> y) -> (y, varsC y) -> Rules a c x -> State ((b, predsC b), (d, symsC d)) (Rules b d y)
transformRules nextPred nextSym nextVar vars = mapM (transformRule nextPred nextSym nextVar vars)

transformAtom :: (Eq a, Eq c, Eq x, KC.KeyedCollection predsC a, KC.KeyedCollection symsC c, KC.KeyedCollection varsC x) =>
                 (b -> b) -> (d -> d) -> (y -> y) -> Atom a c x -> TransformState predsC symsC varsC b d y (Atom b d y)
transformAtom nextPred nextSym nextVar = mapAtomM (transformPreds nextPred) (transformSyms nextSym) (transformVars nextVar)

transformAtoms :: (Eq a, Eq c, Eq x, KC.KeyedCollection predsC a, KC.KeyedCollection symsC c, KC.KeyedCollection varsC x) =>
                  (b -> b) -> (d -> d) -> (y -> y) -> Atoms a c x -> TransformState predsC symsC varsC b d y (Atoms b d y)
transformAtoms nextPred nextSym nextVar = mapM (transformAtom nextPred nextSym nextVar)

transformQuery :: (Eq a, Eq c, Eq x, KC.KeyedCollection predsC a, KC.KeyedCollection symsC c, KC.KeyedCollection varsC x) =>
                  (b -> b) -> (d -> d) -> (y -> y) -> Atoms a c x -> TransformState predsC symsC varsC b d y (Atoms b d y)
transformQuery nextPred nextSym nextVar query = state $ \s@(preds, _, _) -> let m = transformAtoms nextPred nextSym nextVar query
                                                                                (query', (_, syms', vars')) = runState m s
                                                                            in (query', (preds, syms', vars'))

createDataBase :: (Eq a, Eq c, Eq x, Eq b, KC.KeyedCollection predsC a, KC.KeyedCollection symsC c, KC.KeyedCollection varsC x, KC.KeyedCollection dbC b, Functor dbC) =>
            (b -> b) -> (d -> d) -> (y -> y) -> ((b, predsC b), (d, symsC d)) -> (y, varsC y) ->
            Rules a c x -> (dbC (Rules b d y), ((b, predsC b), (d, symsC d)))
createDataBase nextPred nextSym nextVar st vars rules = let m = transformRules nextPred nextSym nextVar vars rules
                                                            (rules', st') = runState m st
                                                        in (insertIntoDB rules', st')
    where insertIntoDB rules = fmap reverse $ execState (mapM_ insertRule rules) KC.empty 
          insertRule rule = state $ \db -> let p = predSymbol $ rhead rule
                                           in ((), KC.insert db p $ case KC.find db p of
                                                                        Nothing -> [rule]
                                                                        Just rs -> rule:rs) 
