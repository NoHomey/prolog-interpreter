module Prolog.Parse.Grammar (
    NonTerminal(..),
    Terminal,
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

prefixRule :: n -> n -> G.Rule n t
prefixRule p r = G.Rule [[G.NonTerminal p, G.NonTerminal r], [G.NonTerminal p]]

terminalRule :: [t] -> G.Rule n t
terminalRule ts = G.Rule [[G.Terminal t] | t <- ts]

whiteSpaceSkip :: n -> t -> G.Rule n t
whiteSpaceSkip s t = G.Rule [[G.Skip s, G.Terminal t, G.Skip s], [G.Skip s, G.Terminal t], [G.Terminal t, G.Skip s], [G.Terminal t]]

start = alternativeRecursiveRule Fact Rule Start

fact = singleChoiceRule [G.NonTerminal Atom, G.Skip Dot]

rule = singleChoiceRule [G.NonTerminal Atom, G.Skip Dots, G.Skip Dash, G.NonTerminal Atoms, G.Skip Dot]

atom = singleChoiceRule [G.NonTerminal Identifier, G.Skip LeftParenthesis, G.NonTerminal Terms, G.Skip RightParenthesis]

atoms = listRule Atom Comma Atoms

term = choiceRule [Func, Const, Var]

terms = listRule Term Comma Terms

func = singleChoiceRule [G.NonTerminal Identifier, G.Skip LeftParenthesis, G.NonTerminal Terms, G.Skip RightParenthesis]

const = choiceRule [Identifier]

identifier = prefixRule LowerCaseLetter LetterOrDigit

var = prefixRule UpperCaseLetter LetterOrDigit

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

whiteSpaces = recursiveRule WhiteSpace WhiteSpaces

whiteSpace = terminalRule ['\n', '\t', ' ']

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