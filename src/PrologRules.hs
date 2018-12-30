module PrologRules (
    Identifier,
    Term(..),
    Atom(..),
    Rule(..),
    PrologParseTree,
    Rules,
    rules,
    fArity,
    pArity,
    tmap
) where

import qualified Grammar as G
import qualified ParseTree as PT
import qualified PrologGrammar as PG
import Data.List

type Identifier = [PG.E]

data Term s v = Const s | Var v | Func {funcSymbol :: s, params :: [Term s v]} deriving (Eq)

data Atom s v = Atom {predSymbol :: s, terms :: [Term s v]}

data Rule s v = Rule {rhead :: Atom s v, body :: [Atom s v]}

type PrologParseTree = PT.ParseTree PG.GVars PG.E

type Rules s v = [Rule s v]

instance Functor (Term s) where
    fmap f t = case t of
                   (Var y) -> Var $ f y
                   (Const c) -> Const c
                   (Func s ps) -> Func {funcSymbol = s, params = map (fmap f) ps}

instance (Show s, Show v) => Show (Term s v) where
    show (Const id) = show id
    show (Var id) = show id
    show f = (show $ funcSymbol f) ++ (show $ params f)

instance (Show s, Show v) => Show (Atom s v) where
    show a = (show $ predSymbol a) ++ (show $ terms a)

instance (Show s, Show v) => Show (Rule s v) where
    show r = (show $ rhead r) ++ if null $ body r then "" else (" <- " ++ (show $ body r))

onlyChild :: PrologParseTree -> PrologParseTree
onlyChild pt = let [c] = PT.children pt in c

firstChild :: PrologParseTree -> PrologParseTree
firstChild pt = head $ PT.children pt

secondChild :: PrologParseTree -> PrologParseTree
secondChild pt = head $ tail $ PT.children pt

terminal :: PrologParseTree -> PG.E
terminal pt = case PT.value pt of
                  G.T t -> t
                  G.N _ -> terminal $ onlyChild pt

recTransform :: (PrologParseTree -> a) -> PrologParseTree -> [a]
recTransform f pt = case PT.children pt of
                           [t, ts] -> (f t):(recTransform f ts)
                           [t] -> [f t]

identifier :: PrologParseTree -> Identifier
identifier = recTransform terminal

var :: PrologParseTree -> Identifier
var = recTransform terminal

term :: PrologParseTree -> Term Identifier Identifier
term pt = let child = onlyChild pt in case PT.value child of          
              G.N PG.Var -> Var $ var $ child
              G.N PG.Const -> Const $ identifier $ onlyChild child
              G.N PG.Func -> Func {funcSymbol = identifier $ firstChild child, params = listOfTerms $ secondChild child}

listOfTerms :: PrologParseTree -> [Term Identifier Identifier]
listOfTerms = recTransform term

atom :: PrologParseTree -> Atom Identifier Identifier
atom pt = Atom {predSymbol = identifier $ firstChild pt, terms = listOfTerms $ secondChild pt}

atoms :: PrologParseTree -> [Atom Identifier Identifier]
atoms = recTransform atom

rule :: PrologParseTree -> Rule  Identifier Identifier
rule r = case PT.value r of
             G.N PG.Fact -> Rule {rhead = atom $ onlyChild r, body = []}
             G.N PG.Rule -> Rule {rhead = atom $ firstChild r, body = atoms $ secondChild r}

rules :: PrologParseTree -> Rules Identifier Identifier
rules = recTransform rule

fArity :: Term s v -> Int
fArity (Func _ params) = length params
fArity _ = 0

pArity :: Atom s v -> Int
pArity = length . terms

tmap :: (a -> Term s b) -> Term s a -> Term s b
tmap f t = case t of
               (Var y) -> f y
               (Const c) -> Const c
               (Func s ps) -> Func {funcSymbol = s, params = map (tmap f) ps}