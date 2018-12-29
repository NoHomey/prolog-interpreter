module Grammar (
    Symbol(..),
    RuleBody,
    Rule,
    Grammar,
    body,
    rules,
    rule,
    recRule,
    alternativeRecRule,
    singleChoiceRule,
    choiceRule,
    listRule,
    startsWithRule,
    terminalRule,
    grammar) where

import Data.Maybe

data Symbol n t = N n | T t | S t deriving (Eq, Show)

type RuleBody n t = [Symbol n t]

newtype Rule n t = Rule {body :: [RuleBody n t]}

newtype Grammar n t = Grammar {rules :: n -> Rule n t}

rule :: [RuleBody n t] -> Rule n t
rule bs = Rule {body = bs}

recRule :: n -> n -> Rule n t
recRule t r = rule [[N t, N r], [N t]]

alternativeRecRule :: n -> n -> n -> Rule n t
alternativeRecRule t1 t2 r = rule [[N t1, N r], [N t2, N r], [N t1], [N t2]]

singleChoiceRule :: RuleBody n t -> Rule n t
singleChoiceRule b = rule [b]

choiceRule :: [n] -> Rule n t
choiceRule ns = rule [[N n] | n <- ns]

listRule :: n -> t -> n -> Rule n t
listRule t s r = rule [[N t, S s, N r], [N t]]

startsWithRule :: n -> n -> Rule n t
startsWithRule p r = rule [[N p, N r], [N p]]

terminalRule :: [t] -> Rule n t
terminalRule ts = rule [[T t] | t <- ts]

grammar :: (n -> Rule n t) -> Grammar n t
grammar rs = Grammar {rules = rs}