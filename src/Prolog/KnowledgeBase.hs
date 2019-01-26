{-# LANGUAGE ConstraintKinds #-}

module Prolog.KnowledgeBase (
    Next,
    RenameInfo,
    AtomRenameInfo,
    RuleRenameInfo,
    KnowledgeBase,
    Nexts,
    RenameAtomConstraint,
    KnowledgeBaseConstraint,
    KnowledgeBaseCreationConstraint,
    renameQuery,
    createKnowledgeBase
) where

import qualified Prolog.Types as T
import qualified Control.Monad.State as S
import qualified Data.KeyedCollection as KC
import qualified Data.Bifunctor as Bf
import qualified Trifunctor as Tf
import FunctorM
import Data.Maybe

type Next a = a -> a

type RenameInfo a c = (a, c a)

type Rename a c = (a, RenameInfo a c)

type RenameState p s v r = S.State (p, s, v) r

type AtomRenameInfo p predsC s symsC v varsC = (RenameInfo p predsC, RenameInfo s symsC, RenameInfo v varsC)

type RuleRenameInfo p predsC s symsC = (RenameInfo p predsC, RenameInfo s symsC)

type AtomRenameState p predsC s symsC v varsC t = S.State (AtomRenameInfo p predsC s symsC v varsC) (t p s v)

type RenameRuleState p predsC s symsC v = S.State (RuleRenameInfo p predsC s symsC) (T.Rule p s v)

type RenamedQueryInfo p predsC s symsC v varsC = (T.Query p s v, AtomRenameInfo p predsC s symsC v varsC)

type KnowledgeBase kb p s v = kb (T.Rules p s v)

type Nexts p s v = (Next p, Next s, Next v)

type KnowledgeBaseInfo kb p predsC s symsC v = (KnowledgeBase kb p s v, RuleRenameInfo p predsC s symsC)

type RenameConstraint c k = (Eq k, KC.KeyedCollection c k)

type RenameAtomConstraint p predsC s symsC v varsC = (RenameConstraint predsC p, RenameConstraint symsC s , RenameConstraint varsC v)

type KnowledgeBaseConstraint kb k = (KC.KeyedCollection kb k, Functor kb)

type KnowledgeBaseCreationConstraint p predsC s symsC v varsC kb k = (RenameAtomConstraint p predsC s symsC v varsC, KnowledgeBaseConstraint kb k, Eq k)

rename :: RenameConstraint c a => Next b -> RenameInfo b c -> a -> Rename b c
rename next info id = maybe (makeRename info) (renameInfo info) $ KC.find (snd info) id
    where makeRename (last, known) = (last, (next last, KC.insert known id last))
          renameInfo = flip (,)

renamePred :: RenameConstraint predsC p => Next p' -> p -> RenameState (RenameInfo p' predsC) s v p'
renamePred nextPred sym = S.state $ \(p, s, v) -> Bf.second (\p' -> (p', s, v)) $ rename nextPred p sym

renameSym :: RenameConstraint symsC s => Next s' -> s -> RenameState p (RenameInfo s' symsC) v s'
renameSym nextSym sym = S.state $ \(p, s, v) -> Bf.second (\s' -> (p, s', v)) $ rename nextSym s sym

renameVar :: RenameConstraint varsC v => Next v' -> v -> RenameState p s (RenameInfo v' varsC) v'
renameVar nextVar x = S.state $ \(p, s, v) -> Bf.second (\v' -> (p, s, v')) $ rename nextVar v x

renameType :: (RenameAtomConstraint p predsC s symsC v varsC, TrifunctorM t)
           => Nexts p' s' v' -> t p s v -> AtomRenameState p' predsC s' symsC v' varsC t
renameType (nextPred, nextSym, nextVar) = trimapM (renamePred nextPred) (renameSym nextSym) (renameVar nextVar)

renameAtom :: RenameAtomConstraint p predsC s symsC v varsC
           => Nexts p' s' v' -> T.Atom p s v -> AtomRenameState p' predsC s' symsC v' varsC T.Atom
renameAtom = renameType

renameRule :: RenameAtomConstraint p predsC s symsC v varsC
           => Nexts p' s' v' -> RenameInfo v' varsC -> T.Rule p s v -> RenameRuleState p' predsC s' symsC v'
renameRule nexts v r = let m = renameType nexts r
                       in do 
                            st <- S.get
                            let (r, st') = S.runState m $ addVarsInfo st
                            S.put $ removeVarsInfo st'
                            return r
        where addVarsInfo (p, s) = (p, s, v)                                    
              removeVarsInfo (p, s, _) = (p, s)

renameQuery :: RenameAtomConstraint p predsC s symsC v varsC
            => Nexts p' s' v'
            -> AtomRenameInfo p' predsC s' symsC v' varsC
            -> T.Query p s v
            -> RenamedQueryInfo p' predsC s' symsC v' varsC
renameQuery nexts st@(p, _, _) q = let m = mapM (renameAtom nexts) q
                                   in Bf.second (Tf.first $ const p) $ S.runState m st

createKnowledgeBase :: KnowledgeBaseCreationConstraint p predsC s symsC v varsC rulesC p'
                    => Nexts p' s' v'
                    -> RuleRenameInfo p' predsC s' symsC
                    -> RenameInfo v' varsC
                    -> T.Rules p s v
                    -> KnowledgeBaseInfo rulesC p' predsC s' symsC v'
createKnowledgeBase nexts st v rs = let m = mapM (renameRule nexts v) rs
                                    in Bf.first knowledgeBase $ S.runState m st
    where knowledgeBase rs = fmap reverse $ S.execState (mapM_ insertRule rs) KC.empty
          insertRule r = let p = T.predSymbol $ T.ruleHead r
                         in do
                              kb <- S.get
                              S.put $ KC.insert kb p $ addRuleToDB kb p r
          addRuleToDB kb p r = maybe [r] (r:) $ KC.find kb p