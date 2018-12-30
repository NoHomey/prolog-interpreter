module ParseTree (ParseTree, value, children, parse) where

import Grammar
import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.State

data ParseTree n t = ParseTree {value :: Symbol n t, children :: [ParseTree n t]} deriving (Eq, Show)

type ParseResult n t = State [t] (Maybe (ParseTree n t))

parseTree :: Symbol n t -> [ParseTree n t] -> ParseTree n t
parseTree s cs = ParseTree {value = s, children = cs}

parseTreeLeaf :: Symbol n t -> ParseTree n t
parseTreeLeaf s = parseTree s []

parseTerminalSymbol :: (Eq t) => (t -> Symbol n t) -> t -> ParseResult n t
parseTerminalSymbol s t = state $ \w -> if null w || (head w) /= t then (Nothing, w) else (Just $ parseTreeLeaf $ s t, tail w)

parseTerminal :: (Eq t) => t -> ParseResult n t
parseTerminal = parseTerminalSymbol T 

parseSkipTerminal :: (Eq t) => Maybe [t] -> t -> ParseResult n t
parseSkipTerminal Nothing t = parseTerminalSymbol S t
parseSkipTerminal (Just ss) t = state $ \w -> let (mt, w') = runState (parseTerminalSymbol S t) (skip w) in (mt, skip w')
    where skip = dropWhile (`elem` ss)

parseNonTerminal :: (Eq t) => Grammar n t -> Maybe [t] -> n -> ParseResult n t
parseNonTerminal g ms n = parse $ N n 
    where parse (T t) = parseTerminal t
          parse (S t) = parseSkipTerminal ms t
          parse (N n) = state $ \s -> case tryApply (body $ rules g n) s of
                                          Just (cs, s') -> (Just $ parseTree (N n) cs, s')
                                          Nothing -> (Nothing, s)                                 
          apply [] s = Just ([], s)
          apply (x:xs) s = let (mt, s') = runState (parse x) s
                           in do
                                tree <- mt                                   
                                (cs, s'') <- apply xs s'
                                return (if isSkipTerminal $ value tree then cs else tree:cs, s'')
          tryApply [] s = Nothing
          tryApply (b:bs) s = let m = apply b s
                              in if isJust m then m else tryApply bs s
          isSkipTerminal (S t) = True
          isSkipTerminal _ = False

parse :: (Eq t) => Grammar n t -> Maybe [t] -> n -> [t] -> Maybe (ParseTree n t)
parse g ms s w = evalState (parseNonTerminal g ms s) w