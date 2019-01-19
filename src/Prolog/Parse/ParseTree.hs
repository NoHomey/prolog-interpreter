module Prolog.Parse.ParseTree (
    ParseTree,
    Counter,
    PT.symbol,
    PT.children,
    parse
) where

import Prolog.Parse.Grammar
import qualified Parse.ParseTree as PT

type ParseTree = PT.ParseTree NonTerminal Terminal

type Counter = (Int, Int)

update :: Counter -> Terminal -> Counter 
update (line, symbol) t = if t == '\n'
                            then (line + 1, 1)
                            else (line, symbol + 1)
                            
parse :: NonTerminal -> [Terminal] -> Either [Counter] [ParseTree]
parse = PT.parse grammar update (1, 1)