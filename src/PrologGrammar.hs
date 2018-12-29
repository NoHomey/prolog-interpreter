module PrologGrammar (GVars(..), E, prologGrammar) where

import Grammar as G

data GVars = Start | Id | Var | Const | Atom | Atoms | Term | Terms | Func | Fact | Rule | LCL | UCL | L | D | LDS deriving (Eq, Show)
type E = Char

rStart = G.alternativeRecRule Fact Rule Start
rFact = G.singleChoiceRule [G.N Atom, G.S '.']
rRule = G.singleChoiceRule [G.N Atom, G.S ':', G.S '-', G.N Atoms, G.S '.']
rAtom = G.singleChoiceRule [G.N Id, G.S '(', G.N Terms, G.S ')']
rAtoms = G.listRule Atom ',' Atoms
rTerm = G.choiceRule [Func, Const, Var]
rTerms = G.listRule Term ',' Terms
rFunc = G.singleChoiceRule [G.N Id, G.S '(', G.N Terms, G.S ')']
rConst = G.choiceRule [Id]
rId = G.startsWithRule LCL LDS
rVar = G.startsWithRule UCL LDS
rLDS = G.alternativeRecRule L D LDS
rL = G.choiceRule [LCL, UCL]
rLCL = G.terminalRule ['a'..'z']
rUCL = G.terminalRule ['A'..'Z']
rD = G.terminalRule ['0'..'9']

r :: GVars -> G.Rule GVars E
r Start = rStart
r Fact = rFact
r Rule = rRule
r Atom = rAtom
r Atoms = rAtoms
r Term = rTerm
r Terms = rTerms
r Func = rFunc
r Const = rConst
r Id = rId
r Var = rVar
r LDS = rLDS
r L = rL
r LCL = rLCL
r UCL = rUCL
r D = rD

prologGrammar = G.grammar r