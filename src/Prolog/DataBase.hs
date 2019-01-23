module Prolog.DataBase (
    Next,
    RenameInfo,
    renameQuery,
    createDataBase
) where

import qualified Prolog.Types as T
import qualified Control.Monad.State as S
import qualified Data.KeyedCollection as KC
import FunctorM
import Data.Maybe
import Data.Bifunctor
import Trifunctor

type Next a = a -> a

type RenameInfo a c = (a, c a)

type Rename a c = (a, RenameInfo a c)

type RenameState p s v r = S.State (p, s, v) r

type AtomRenameInfo p predsC s symsC v varsC = (RenameInfo p predsC, RenameInfo s symsC, RenameInfo v varsC)

type RuleRenameInfo p predsC s symsC = (RenameInfo p predsC, RenameInfo s symsC)

type AtomRenameState p predsC s symsC v varsC t = S.State (AtomRenameInfo p predsC s symsC v varsC) (t p s v)

type RenameRuleState p predsC s symsC v = S.State (RuleRenameInfo p predsC s symsC) (T.Rule p s v)

type RenamedQueryInfo p predsC s symsC v varsC = (T.Query p s v, AtomRenameInfo p predsC s symsC v varsC)

type DataBaseInfo db p predsC s symsC v = (db (T.Rules p s v), RuleRenameInfo p predsC s symsC)

rename :: (Eq a, KC.KeyedCollection c a) => Next b -> RenameInfo b c -> a -> Rename b c
rename next info id = maybe (makeRename info) (renameInfo info) $ KC.find (snd info) id
    where makeRename (last, known) = (last, (next last, KC.insert known id last))
          renameInfo = flip (,)

renamePred :: (Eq p, KC.KeyedCollection predsC p) => Next p' -> p -> RenameState (RenameInfo p' predsC) s v p'
renamePred nextPred sym = S.state $ \(p, s, v) -> bimap id (\p' -> (p', s, v)) $ rename nextPred p sym

renameSym :: (Eq s, KC.KeyedCollection symsC s) => Next s' -> s -> RenameState p (RenameInfo s' symsC) v s'
renameSym nextSym sym = S.state $ \(p, s, v) -> bimap id (\s' -> (p, s', v)) $ rename nextSym s sym

renameVar :: (Eq v, KC.KeyedCollection varsC v) => Next v' -> v -> RenameState p s (RenameInfo v' varsC) v'
renameVar nextVar x = S.state $ \(p, s, v) -> bimap id (\v' -> (p, s, v')) $ rename nextVar v x

renameType ::
           ( Eq p
           , Eq s
           , Eq v
           , KC.KeyedCollection predsC p
           , KC.KeyedCollection symsC s
           , KC.KeyedCollection varsC v
           , TrifunctorM t
           )
           => Next p'
           -> Next s'
           -> Next v'
           -> t p s v
           -> AtomRenameState p' predsC s' symsC v' varsC t
renameType nextPred nextSym nextVar = trimapM (renamePred nextPred) (renameSym nextSym) (renameVar nextVar)

renameAtom ::
           ( Eq p
           , Eq s
           , Eq v
           , KC.KeyedCollection predsC p
           , KC.KeyedCollection symsC s
           , KC.KeyedCollection varsC v
           )
           => Next p'
           -> Next s'
           -> Next v'
           -> T.Atom p s v
           -> AtomRenameState p' predsC s' symsC v' varsC T.Atom
renameAtom = renameType

renameRule ::
           ( Eq p
           , Eq s
           , Eq v
           , KC.KeyedCollection predsC p
           , KC.KeyedCollection symsC s
           , KC.KeyedCollection varsC v
           )
           => Next p'
           -> Next s'
           -> Next v'
           -> RenameInfo v' varsC
           -> T.Rule p s v
           -> RenameRuleState p' predsC s' symsC v'
renameRule nextPred nextSym nextVar v r = let m = renameType nextPred nextSym nextVar r
                                          in do 
                                               st <- S.get
                                               let (r, st') = S.runState m $ addVarsInfo st
                                               S.put $ removeVarsInfo st'
                                               return r
        where addVarsInfo (p, s) = (p, s, v)                                    
              removeVarsInfo (p, s, _) = (p, s)

renameQuery ::
            ( Eq p
            , Eq s
            , Eq v
            , KC.KeyedCollection predsC p
            , KC.KeyedCollection symsC s
            , KC.KeyedCollection varsC v
            )
            => Next p'
            -> Next s'
            -> Next v'
            -> AtomRenameInfo p' predsC s' symsC v' varsC
            -> T.Query p s v
            -> RenamedQueryInfo p' predsC s' symsC v' varsC
renameQuery nextPred nextSym nextVar st@(p, _, _) q = let m = mapM (renameAtom nextPred nextSym nextVar) q
                                                      in bimap id (trimap (const p) id id) $ S.runState m st

createDataBase ::
               ( Eq p'
               , Eq p
               , Eq s
               , Eq v
               , KC.KeyedCollection predsC p
               , KC.KeyedCollection symsC s
               , KC.KeyedCollection varsC v
               , KC.KeyedCollection rulesC p'
               , Functor rulesC
               )
               => Next p'
               -> Next s'
               -> Next v'
               -> RuleRenameInfo p' predsC s' symsC
               -> RenameInfo v' varsC
               -> T.Rules p s v
               -> DataBaseInfo rulesC p' predsC s' symsC v'
createDataBase np ns nv st v rs = let m = mapM (renameRule np ns nv v) rs
                                  in bimap dataBase id $ S.runState m st
    where dataBase rs = fmap reverse $ S.execState (mapM_ insertRule rs) KC.empty
          insertRule r = let p = T.predSymbol $ T.ruleHead r
                         in do
                              db <- S.get
                              S.put $ KC.insert db p $ addRuleToDB db p r
          addRuleToDB db p r = maybe [r] (r:) $ KC.find db p