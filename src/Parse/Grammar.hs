module Parse.Grammar (
    Symbol(..),
    Rule(..),
    Grammar(..),
    RuleBody
) where

data Symbol n t = Skip n | NonTerminal n | Terminal t deriving (Eq, Show)

type RuleBody n t = [Symbol n t]

newtype Rule n t = Rule {body :: [RuleBody n t]}

newtype Grammar n t = Grammar {rules :: n -> Rule n t}