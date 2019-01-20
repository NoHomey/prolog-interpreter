module Prolog.DataBase (
    RenameInfo,
    renameQuery,
    createDataBase
) where

import qualified Prolog.Types as T
import qualified Control.Monad.State as S
import qualified Data.KeyedCollection as KC
import FunctorM

type Next a = a -> a

type RenameInfo a c = (a, c a)

type Rename a c = (a, RenameInfo a c)

rename :: (Eq a, KC.KeyedCollection c a) => Next b -> RenameInfo b c -> a -> Rename b c
rename next (last, info) id = case KC.find info id of
                                  Nothing -> result last (next last) $ KC.insert info id last
                                  (Just renamed) -> result renamed last info
    where result renamed next info = (renamed, (next, info))

renamePred :: (Eq p, KC.KeyedCollection predsC p) => Next p' -> p -> S.State (RenameInfo p' predsC, s, v) p'
renamePred nextPred id = S.state $ \(p, s, v) -> let (r, p') = rename nextPred p id
                                                 in (r, (p', s, v))

renameSym :: (Eq s, KC.KeyedCollection symsC s) => Next s' -> s -> S.State (p, RenameInfo s' symsC, v) s'
renameSym nextSym id = S.state $ \(p, s, v) -> let (r, s') = rename nextSym s id
                                               in (r, (p, s', v))

renameVar :: (Eq v, KC.KeyedCollection varsC v) => Next v' -> v -> S.State (p, s, RenameInfo v' varsC) v'
renameVar nextVar id = S.state $ \(p, s, v) -> let (r, v') = rename nextVar v id
                                               in (r, (p, s, v'))

renameType :: (Eq p, Eq s, Eq v, KC.KeyedCollection predsC p, KC.KeyedCollection symsC s, KC.KeyedCollection varsC v, TrifunctorM t)
           => Next p'
           -> Next s'
           -> Next v'
           -> t p s v
           -> S.State (RenameInfo p' predsC, RenameInfo s' symsC, RenameInfo v' varsC) (t p' s' v')
renameType nextPred nextSym nextVar = trimapM (renamePred nextPred) (renameSym nextSym) (renameVar nextVar)

renameAtom :: (Eq p, Eq s, Eq v, KC.KeyedCollection predsC p, KC.KeyedCollection symsC s, KC.KeyedCollection varsC v)
           => Next p'
           -> Next s'
           -> Next v'
           -> T.Atom p s v
           -> S.State (RenameInfo p' predsC, RenameInfo s' symsC, RenameInfo v' varsC) (T.Atom p' s' v')
renameAtom = renameType

renameRule :: (Eq p, Eq s, Eq v, KC.KeyedCollection predsC p, KC.KeyedCollection symsC s, KC.KeyedCollection varsC v)
           => Next p'
           -> Next s'
           -> Next v'
           -> RenameInfo v' varsC
           -> T.Rule p s v
           -> S.State (RenameInfo p' predsC, RenameInfo s' symsC) (T.Rule p' s' v')
renameRule nextPred nextSym nextVar v r = S.state action
    where action (p, s) = let (r', (p', s', _)) = S.runState (renameType nextPred nextSym nextVar r) (p, s, v)
                          in (r', (p', s'))

renameQuery :: (Eq p, Eq s, Eq v, KC.KeyedCollection predsC p, KC.KeyedCollection symsC s, KC.KeyedCollection varsC v)
            => Next p'
            -> Next s'
            -> Next v'
            -> T.Query p s v
            -> S.State (RenameInfo p' predsC, RenameInfo s' symsC, RenameInfo v' varsC) (T.Query p' s' v')
renameQuery nextPred nextSym nextVar q = S.state $ \st@(p, _, _) -> let m = mapM (renameAtom nextPred nextSym nextVar) q
                                                                        (q', (_, s', v')) = S.runState m st
                                                                    in (q', (p, s', v'))

createDataBase :: (Eq p', Eq p, Eq s, Eq v, KC.KeyedCollection predsC p, KC.KeyedCollection symsC s, KC.KeyedCollection varsC v, KC.KeyedCollection rulesC p', Functor rulesC)
               => Next p'
               -> Next s'
               -> Next v'
               -> (RenameInfo p' predsC, RenameInfo s' symsC)
               -> RenameInfo v' varsC
               -> T.Rules p s v
               -> (rulesC (T.Rules p' s' v'), (RenameInfo p' predsC, RenameInfo s' symsC))
createDataBase np ns nv st v rs = let m = mapM (renameRule np ns nv v) rs
                                      (rs', st') = S.runState m st
                                  in (insertIntoDB rs', st')
    where insertIntoDB rs = fmap reverse $ S.execState (mapM_ insertRule rs) KC.empty
          insertRule r = S.state $ \db -> let p = T.predSymbol $ T.ruleHead r
                                          in ((), KC.insert db p $ addRuleToDB db p r)
          addRuleToDB db p r = case KC.find db p of
                                   Nothing -> [r]
                                   (Just rs) -> r:rs