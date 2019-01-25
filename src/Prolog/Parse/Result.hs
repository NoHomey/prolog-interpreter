module Prolog.Parse.Result (
    Identifier,
    Term,
    Terms,
    Atom,
    Atoms,
    Query,
    Rule,
    Rules,
    rules,
    query
) where

import qualified Prolog.Parse.ParseTree as PT
import qualified Parse.Grammar as G
import qualified Prolog.Parse.Grammar as PG
import qualified Prolog.Types as T

type Identifier = [PG.Terminal]

type Term = T.Term Identifier Identifier

type Terms = [Term]

type Atom = T.Atom Identifier Identifier Identifier

type Atoms = [Atom]

type Query = Atoms

type Rule = T.Rule Identifier Identifier Identifier

type Rules = [Rule]

onlyChild :: PT.ParseTree -> PT.ParseTree
onlyChild pt = let [c] = PT.children pt in c

firstChild :: PT.ParseTree -> PT.ParseTree
firstChild = head . PT.children

secondChild :: PT.ParseTree -> PT.ParseTree
secondChild = head . tail . PT.children

terminal :: PT.ParseTree -> PG.Terminal
terminal pt = case PT.symbol pt of
                  G.Terminal t -> t
                  G.NonTerminal _ -> terminal $ onlyChild pt

recTransform :: (PT.ParseTree -> a) -> PT.ParseTree -> [a]
recTransform f pt = case PT.children pt of
                        [t, ts] -> (f t):recTransform f ts
                        [t] -> [f t]

identifier :: PT.ParseTree -> Identifier
identifier = recTransform terminal

var :: PT.ParseTree -> Identifier
var = recTransform terminal

term :: PT.ParseTree -> Term
term pt = let child = onlyChild pt
          in case PT.symbol child of          
                 (G.NonTerminal PG.Var) -> T.Var $ var $ child
                 (G.NonTerminal PG.Const) -> T.Func (identifier $ onlyChild child) []
                 (G.NonTerminal PG.Func) -> T.Func (identifier $ firstChild child) (listOfTerms $ secondChild child)

listOfTerms :: PT.ParseTree -> Terms
listOfTerms = recTransform term

atom :: PT.ParseTree -> Atom
atom pt = T.Atom (identifier $ firstChild pt) (listOfTerms $ secondChild pt)

atoms :: PT.ParseTree -> Atoms
atoms = recTransform atom

rule :: PT.ParseTree -> Rule
rule r = case PT.symbol r of
             (G.NonTerminal PG.Fact) -> T.Rule (atom $ onlyChild r) []
             (G.NonTerminal PG.Rule) -> T.Rule (atom $ firstChild r) (atoms $ secondChild r)

rules :: PT.ParseTree -> Rules
rules = recTransform rule

query :: PT.ParseTree -> Query
query = atoms