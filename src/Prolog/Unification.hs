module Prolog.Unification (
     Substitution,
     SubstitutionFunc,
     empty,
     vars,
     substitute,
     substitutionFuncForVar,
     substitutionFuncFromSubstitution,
     compose,
     unify
) where

import qualified Prolog.OptimizeForUnification as OU
import qualified Prolog.Types as T 
import Control.Monad
import Data.Function
import Data.List
import Data.Bifunctor
import Data.Maybe

type Substitution s v = [(v, OU.Term s v)]

type SubstitutionFunc s v = v -> Maybe (OU.Term s v)

type Equations s v = [(OU.Term s v, OU.Term s v)]

empty :: Substitution s v
empty = []

vars :: OU.Term s v -> [v]
vars (T.Var x) = [x]
vars (T.Func f ps) = concatMap vars ps

substitute :: SubstitutionFunc s v -> OU.Term s v -> OU.Term s v
substitute s t@(T.Var y)   = maybe t id $ s y
substitute s (T.Func f ps) = T.Func f $ map (substitute s) ps

substitutionFuncForVar :: (Eq v) => v -> OU.Term s v -> SubstitutionFunc s v
substitutionFuncForVar x t y = if x == y then Just t else Nothing

substitutionFuncFromSubstitution :: (Eq v) => Substitution s v -> SubstitutionFunc s v
substitutionFuncFromSubstitution s x = fmap (substitute (substitutionFuncFromSubstitution s) . snd) $ find ((x ==) . fst) s

compose :: (Eq v) => Substitution s v -> Substitution s v -> Substitution s v
compose uf ug = let composed = map (fmap $ substitute $ substitutionFuncFromSubstitution uf) ug
                    notIncluded = filter (isNothing . substitutionFuncFromSubstitution ug . fst) uf
                in composed ++ notIncluded

tryUnify :: (Eq s, Eq v) => Equations s v -> Maybe (Substitution s v)
tryUnify []            = Just empty
tryUnify (e@(a, b):es) = case e of
                             (T.Func _ _, T.Var _) -> tryUnify $ (b, a):es
                             (T.Var x, T.Func _ _) -> if x `elem` (vars b)
                                                        then Nothing
                                                        else eliminate x b es
                             (T.Var x, T.Var y) -> if x == y
                                                     then tryUnify es
                                                     else eliminate x b es
                             (T.Func f fps, T.Func g gps) -> if f == g
                                                               then tryUnify $ (zip fps gps) ++ es
                                                               else Nothing
     where eliminate x t es = do
                                let sub = substitute (substitutionFuncForVar x t)
                                s <- tryUnify $ map (bimap sub sub) es
                                return $ (x, t):s

unify :: (Eq p, Eq s, Eq v) => OU.Atom p s v -> OU.Atom p s v -> Maybe (Substitution s v)
unify a b = do
              guard $ ((==) `on` T.predSymbol) a b
              s <- tryUnify $ (zip `on` T.terms) a b
              let sub = substitute (substitutionFuncFromSubstitution s) 
              return $ map (bimap id sub) s