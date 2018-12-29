module PrologDataBase (Term(..), Atom(..), Rule(..), PrologParseTree, PrologDataBase, prologDataBase) where

import Debug.Trace

import qualified Grammar as G
import qualified ParseTree as PT
import qualified PrologGrammar as PG

import Data.List

data Term = Const [PG.E] | Var [PG.E] | Func {funcSymbol :: String, params :: [Term]} deriving (Eq)
data Atom = Atom {predSymbol :: String, terms :: [Term]}
data Rule = Rule {rhead :: Atom, body :: [Atom]}

instance Show Term where
    show (Const id) = id
    show (Var id) = id
    show f = (funcSymbol f) ++ (show $ params f)

instance Show Atom where
    show a = (predSymbol a) ++ (show $ terms a)

instance Show Rule where
    show r = (show $ rhead r) ++ if null $ body r then "" else (" <- " ++ (show $ body r))

type PrologParseTree = PT.ParseTree PG.GVars PG.E

type PrologDataBase = [Rule]

onlyChild :: PrologParseTree -> PrologParseTree
onlyChild pt = let [c] = PT.child pt in c

firstChild :: PrologParseTree -> PrologParseTree
firstChild pt = head $ PT.child pt

secondChild :: PrologParseTree -> PrologParseTree
secondChild pt = head $ tail $ PT.child pt

terminal :: PrologParseTree -> PG.E
terminal pt = case PT.value pt of
                  G.T t -> t
                  G.N _ -> terminal $ onlyChild pt

recTransform :: (PrologParseTree -> a) -> PrologParseTree -> [a]
recTransform f pt = case PT.child pt of
                           [t, ts] -> (f t):(recTransform f ts)
                           [t] -> [f t]

identifier :: PrologParseTree -> [PG.E]
identifier = recTransform terminal

var :: PrologParseTree -> [PG.E]
var = recTransform terminal

term :: PrologParseTree -> Term
term pt = let child = onlyChild pt in case PT.value child of          
              G.N PG.Var -> Var $ var $ child
              G.N PG.Const -> Const $ identifier $ onlyChild child
              G.N PG.Func -> Func {funcSymbol = identifier $ firstChild child, params = listOfTerms $ secondChild child}

listOfTerms :: PrologParseTree -> [Term]
listOfTerms = recTransform term

atom :: PrologParseTree -> Atom
atom pt = Atom {predSymbol = identifier $ firstChild pt, terms = listOfTerms $ secondChild pt}

atoms :: PrologParseTree -> [Atom]
atoms = recTransform atom

rule :: PrologParseTree -> Rule
rule r = case PT.value r of
             G.N PG.Fact -> Rule {rhead = atom $ onlyChild r, body = []}
             G.N PG.Rule -> Rule {rhead = atom $ firstChild r, body = atoms $ secondChild r}

prologDataBase :: PrologParseTree -> PrologDataBase
prologDataBase = recTransform rule