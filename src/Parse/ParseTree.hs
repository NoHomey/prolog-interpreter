module Parse.ParseTree (
    ParseTree,
    symbol,
    children,
    parse
) where

import Parse.Grammar
import Data.List
import Data.Either

data ParseTree n t = ParseTree {symbol :: Symbol n t, children :: [ParseTree n t]} deriving (Eq, Show)

type ParseResult n t s = Either s (ParseTree n t)

type ParsingResult n t s = [(ParseResult n t s, ([t], s))]

parseTreeLeaf :: Symbol n t -> ParseTree n t
parseTreeLeaf s = ParseTree s []

parseTerminal :: (Eq t) => (s -> t -> s) -> t -> ([t], s) -> ParsingResult n t s
parseTerminal u t s@(w, p) = [if null w || (head w) /= t
                                then (Left p, s)
                                else (Right $ parseTreeLeaf $ Terminal t, (tail w, u p t))]

parseEnd :: ([t], s) -> ParsingResult n t s
parseEnd s@(w, p) = [if null w then (Right $ parseTreeLeaf End, s) else (Left p, s)]

parseNonTerminal :: (Eq t) => Grammar n t -> (s -> t -> s) -> n -> ([t], s) -> ParsingResult n t s
parseNonTerminal g u n s = parse (NonTerminal n) s
    where parse (Skip n)         = parseNonTerminalSymbol Skip n
          parse (NonTerminal n)  = parseNonTerminalSymbol NonTerminal n
          parse (Terminal t)     = parseTerminal u t
          parse End              = parseEnd
          parseNonTerminalSymbol symbol n s = case tryApply (body $ rules g n) s of
                                                  (Right rs) -> [(Right $ ParseTree (symbol n) cs, st) | (cs, st) <- rs]
                                                  (Left es) -> es 
          apply state []     = Right [([], state)]
          apply state (x:xs) = let (es, ts) = partition (isLeft . fst) $ parse x state
                               in if null ts
                                    then Left es
                                    else let (es', cs) = partition (isLeft . fst) [(apply s xs, fromRight undefined p) | (p, s) <- ts]
                                         in if null cs
                                              then Left $ concatMap ((fromLeft undefined) . fst) es'
                                              else Right $ if shouldSkip x
                                                             then concatMap ((fromRight undefined) . fst) cs                  
                                                             else [(t:ts, s) | (rts, t) <- cs, (ts, s) <- fromRight undefined rts]
          tryApply bs state = let (es, cs) = partitionEithers $ map (apply state) bs
                              in if null cs
                                   then Left $ concat es
                                   else Right $ concat cs
          shouldSkip (Skip _) = True
          shouldSkip End      = True
          shouldSkip _        = False

parse :: (Eq t) => Grammar n t -> (s -> t -> s) -> s -> n -> [t] -> Either [s] [ParseTree n t]
parse g u p n w = let (es, ps) = partition (isLeft . fst) $ parseNonTerminal g u n (w, p)
                      (ws, rrs) = partition (not . null . fst . snd) ps
                      allEs = (map ((fromLeft undefined) . fst) es) ++ [snd $ snd wrong | wrong <- ws]
                      rs = map ((fromRight undefined) . fst) rrs
                  in if null rs
                        then Left allEs
                        else Right rs