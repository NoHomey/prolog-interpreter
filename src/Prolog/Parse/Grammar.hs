module Prolog.Parse.Grammar (
    NonTerminal(..),
    Terminal,
    whiteSpaceSymbols,
    grammar
) where

import qualified Parse.Grammar as G

data NonTerminal = Start
                 | Identifier
                 | Var
                 | Const
                 | Atom
                 | Atoms
                 | Term
                 | Terms
                 | Func
                 | Fact
                 | Rule
                 | LowerCaseLetter
                 | UpperCaseLetter
                 | Letter
                 | Digit
                 | LetterOrDigit
                 | Dot
                 | Comma
                 | LeftParenthesis
                 | RightParenthesis
                 | Dash
                 | Dots
                 | WhiteSpaces
                 | WhiteSpace deriving (Eq, Show)

type Terminal = Char

recursiveRule :: n -> n -> G.Rule n t
recursiveRule n r = G.Rule [[G.NonTerminal n, G.NonTerminal r], [G.NonTerminal n]]

alternativeRecursiveRule :: n -> n -> n -> G.Rule n t
alternativeRecursiveRule n1 n2 r = G.Rule $ [[G.NonTerminal n, G.NonTerminal r] | n <- [n1, n2]] ++ [[G.NonTerminal n] | n <- [n1, n2]]

singleChoiceRule :: G.RuleBody n t -> G.Rule n t
singleChoiceRule = G.Rule . (:[])

choiceRule :: [n] -> G.Rule n t
choiceRule ns = G.Rule [[G.NonTerminal n] | n <- ns]

listRule :: n -> n -> n -> G.Rule n t
listRule n s r = G.Rule [[G.NonTerminal n, G.Skip s, G.NonTerminal r], [G.NonTerminal n]]

terminalRule :: [t] -> G.Rule n t
terminalRule ts = G.Rule [[G.Terminal t] | t <- ts]

whiteSpaceSkip :: n -> t -> G.Rule n t
whiteSpaceSkip s t = G.Rule [[G.Terminal t, G.Skip s], [G.Terminal t]]

start = G.Rule [[G.NonTerminal Rule, G.NonTerminal Start]
               ,[G.NonTerminal Fact, G.NonTerminal Start]
               ,[G.NonTerminal Rule, G.End]
               ,[G.NonTerminal Fact, G.End]]

fact = singleChoiceRule [G.NonTerminal Atom, G.Skip Dot]

rule = singleChoiceRule [G.NonTerminal Atom, G.Skip Dots, G.Skip Dash, G.NonTerminal Atoms, G.Skip Dot]

atom = singleChoiceRule [G.NonTerminal Identifier, G.Skip LeftParenthesis, G.NonTerminal Terms, G.Skip RightParenthesis]

atoms = listRule Atom Comma Atoms

term = choiceRule [Func, Const, Var]

terms = listRule Term Comma Terms

func = singleChoiceRule [G.NonTerminal Identifier, G.Skip LeftParenthesis, G.NonTerminal Terms, G.Skip RightParenthesis]

const = choiceRule [Identifier]

identifier = G.Rule [[G.NonTerminal LowerCaseLetter, G.NonTerminal LetterOrDigit]
                    ,[G.NonTerminal LowerCaseLetter]
                    ,[G.NonTerminal LowerCaseLetter, G.NonTerminal LetterOrDigit, G.Skip WhiteSpaces]
                    ,[G.NonTerminal LowerCaseLetter, G.Skip WhiteSpaces]]

var = G.Rule [[G.NonTerminal UpperCaseLetter, G.NonTerminal LetterOrDigit]
             ,[G.NonTerminal UpperCaseLetter]
             ,[G.NonTerminal UpperCaseLetter, G.NonTerminal LetterOrDigit, G.Skip WhiteSpaces]
             ,[G.NonTerminal UpperCaseLetter, G.Skip WhiteSpaces]]

letterOrDigit = alternativeRecursiveRule Letter Digit LetterOrDigit

letter = choiceRule [LowerCaseLetter, UpperCaseLetter]

lowerCaseLetter = terminalRule ['a'..'z']

upperCaseLetter = terminalRule ['A'..'Z']

digit = terminalRule ['0'..'9']

dot = whiteSpaceSkip WhiteSpaces '.'

comma = whiteSpaceSkip WhiteSpaces ','

leftParenthesis = whiteSpaceSkip WhiteSpaces '('

rightParenthesis = whiteSpaceSkip WhiteSpaces ')'

dash = whiteSpaceSkip WhiteSpaces '-'

dots = whiteSpaceSkip WhiteSpaces ':'

whiteSpaces = G.Rule [[G.Skip WhiteSpace, G.Skip WhiteSpaces], [G.Skip WhiteSpace]]

whiteSpaceSymbols = ['\n', '\t', ' ']

whiteSpace = terminalRule whiteSpaceSymbols

ruleFor :: NonTerminal -> G.Rule NonTerminal Terminal
ruleFor Start            = start
ruleFor Fact             = fact
ruleFor Rule             = rule
ruleFor Atom             = atom
ruleFor Atoms            = atoms
ruleFor Term             = term
ruleFor Terms            = terms
ruleFor Func             = func
ruleFor Const            = Prolog.Parse.Grammar.const
ruleFor Identifier       = identifier
ruleFor Var              = var
ruleFor LetterOrDigit    = letterOrDigit
ruleFor Letter           = letter
ruleFor LowerCaseLetter  = lowerCaseLetter
ruleFor UpperCaseLetter  = upperCaseLetter
ruleFor Digit            = digit
ruleFor Dot              = dot
ruleFor Comma            = comma
ruleFor LeftParenthesis  = leftParenthesis
ruleFor RightParenthesis = rightParenthesis
ruleFor Dash             = dash
ruleFor Dots             = dots
ruleFor WhiteSpaces      = whiteSpaces
ruleFor WhiteSpace       = whiteSpace

grammar :: G.Grammar NonTerminal Terminal
grammar = G.Grammar ruleFor