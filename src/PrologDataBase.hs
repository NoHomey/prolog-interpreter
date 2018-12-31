--{-# LANGUAGE AllowAmbiguousTypes #-}

module PrologDataBase (
    mapTermM,
    mapAtomM,
    mapRuleM,
    transformPreds,
    transformSyms,
    transformVars,
    transformRule,
    transformRules
) where

import PrologRules
import qualified KeyedCollection as KC
import KeyToPath
import Control.Monad.State

type TransformRuleState predsC symsC varsC c d x = State ((c, predsC c), (c, symsC c), (d, varsC d)) x 

mapTermM :: Monad m => (a -> m c) -> (b -> m d) -> Term a b -> m (Term c d)
mapTermM f g t = case t of
                       (Var x) -> g x >>= (\x' -> return $ Var x')
                       (Const c) -> f c >>= (\c' -> return $ Const c')
                       (Func s ps) -> do
                                        s' <- f s
                                        ps' <- mapM (mapTermM f g) ps
                                        return $ Func {funcSymbol = s', params = ps'}

mapAtomM :: Monad m => (a -> m c) -> (a -> m c) -> (b -> m d) -> Atom a b -> m (Atom c d)
mapAtomM h f g a = do
                      p <- h $ predSymbol a
                      ts <- mapM (mapTermM f g) $ terms a
                      return $ Atom {predSymbol = p, terms = ts}

mapRuleM :: Monad m => (a -> m c) ->  (a -> m c) -> (b -> m d) -> Rule a b -> m (Rule c d)
mapRuleM h f g r = do
                     let m = mapAtomM h f g
                     h <- m $ rhead r
                     bs <- mapM m $ body r
                     return $ Rule {rhead = h, body = bs}

transformPreds :: (Eq a, KC.KeyedCollection predsC a) =>
                 (c -> c) -> a -> TransformRuleState predsC symsC varsC c d c
transformPreds update pred = state action
    where action ((st, col), syms, vars) = case KC.find col pred of
                                                Nothing -> let col' = KC.insert col pred st
                                                               st' = update st
                                                           in (st, ((st', col'), syms, vars))
                                                Just v -> (v, ((st, col), syms, vars))

transformSyms :: (Eq a, KC.KeyedCollection symsC a) =>
                 (c -> c) -> a -> TransformRuleState predsC symsC varsC c d c
transformSyms update sym = state action
    where action (preds, (st, col), vars) = case KC.find col sym of
                                                Nothing -> let col' = KC.insert col sym st
                                                               st' = update st
                                                           in (st, (preds, (st', col'), vars))
                                                Just v -> (v, (preds, (st, col), vars))
                            
transformVars :: (Eq b, KC.KeyedCollection varsC b) =>
                 (d -> d) -> b -> TransformRuleState predsC symsC varsC c d d
transformVars update var = state action
    where action (preds, syms, (st, col)) = case KC.find col var of
                                                Nothing -> let col' = KC.insert col var st
                                                               st' = update st
                                                           in (st, (preds, syms, (st', col')))
                                                Just v -> (v, (preds, syms, (st, col)))

transformRule :: (Eq a, Eq b, KC.KeyedCollection predsC a, KC.KeyedCollection symsC a, KC.KeyedCollection varsC b) =>
                 (c -> c) -> (c -> c) -> (d -> d) -> (d, varsC d) -> Rule a b -> State ((c, predsC c), (c, symsC c)) (Rule c d)
transformRule nextPred nextSym nextVar vars rule = state action
    where action (preds, syms) = let m = mapRuleM (transformPreds nextPred) (transformSyms nextSym) (transformVars nextVar) rule
                                     (rule', (preds', syms', _)) = runState m (preds, syms, vars)
                                 in (rule', (preds', syms'))

transformRules :: (Eq a, Eq b, KC.KeyedCollection predsC a, KC.KeyedCollection symsC a, KC.KeyedCollection varsC b) =>
                  (c -> c) -> (c -> c) -> (d -> d) -> (d, varsC d) -> Rules a b -> State ((c, predsC c), (c, symsC c)) (Rules c d)
transformRules nextPred nextSym nextVar vars = mapM (transformRule nextPred nextSym nextVar vars)