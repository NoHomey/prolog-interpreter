module Prolog.OptimizeForUnification (
    ArityIdentifier(..),
    Term,
    Terms,
    Atom,
    Atoms,
    Query,
    Rule,
    Rules,
    optimizeQueryForUnification,
    optimizeDataBaseForUnification
) where

import qualified Prolog.Types as T
import qualified Data.KeyedCollection as KC

data ArityIdentifier a = ArityIdentifier {identifier :: a, arity :: Int} deriving (Eq)

type Term s v = T.Term (ArityIdentifier s) v

type Terms s v = [Term s v]

type Atom p s v = T.Atom (ArityIdentifier p) (ArityIdentifier s) v

type Atoms p s v = [Atom p s v]

type Query p s v = Atoms p s v

type Rule p s v = T.Rule (ArityIdentifier p) (ArityIdentifier s) v

type Rules p s v = [Rule p s v]

instance (Show a) => Show (ArityIdentifier a) where
    show aid = "{" ++ (show $ identifier aid) ++ ", " ++ (show $ arity aid) ++ "}"

addArity :: a -> [t] -> ArityIdentifier a
addArity id ts = ArityIdentifier id $ length ts

addArityToTerm :: T.Term s v -> Term s v
addArityToTerm (T.Var x)     = T.Var x
addArityToTerm (T.Func f ps) = T.Func (addArity f ps) $ addArityToTerms ps

addArityToTerms :: T.Terms s v -> Terms s v
addArityToTerms = map addArityToTerm

addArityToAtom :: T.Atom p s v -> Atom p s v
addArityToAtom (T.Atom p ts) = T.Atom (addArity p ts) $ addArityToTerms ts

addArityToAtoms :: T.Atoms p s v -> Atoms p s v
addArityToAtoms = map addArityToAtom

addArityToQuery :: T.Query p s v -> Query p s v
addArityToQuery = addArityToAtoms

addArityToRule :: T.Rule p s v -> Rule p s v
addArityToRule (T.Rule a as) = T.Rule (addArityToAtom a) $ addArityToAtoms as

addArityToRules :: T.Rules p s v -> Rules p s v
addArityToRules = map addArityToRule

optimizeQueryForUnification :: T.Query p s v -> Query p s v
optimizeQueryForUnification = addArityToQuery

optimizeDataBaseForUnification :: (Functor dataBase) => dataBase (T.Rules p s v) -> dataBase (Rules p s v)
optimizeDataBaseForUnification = fmap addArityToRules