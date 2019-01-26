module Parse.ParseTree (
    ParseTree,
    UpdateState,
    symbol,
    children,
    parse
) where

import Parse.Grammar
import Data.List
import Data.Either
import Data.Bifunctor

data ParseTree n t = ParseTree {symbol :: Symbol n t, children :: [ParseTree n t]}

type State t s = ([t], s)

type ParseResult n t s = Either [s] [(ParseTree n t, State t s)]

type UpdateState t s = s -> t -> s

parseTreeLeaf :: Symbol n t -> ParseTree n t
parseTreeLeaf s = ParseTree s []

parseError :: s -> ParseResult n t s
parseError s = Left [s]

accept :: ParseTree n t -> State t s -> ParseResult n t s
accept pt s = Right [(pt, s)]

parseTerminal :: (Eq t) => UpdateState t s -> t -> State t s -> ParseResult n t s
parseTerminal u t (w, p) = if null w || (head w) /= t
                             then parseError p
                             else accept (parseTreeLeaf $ Terminal t) (tail w, u p t)

parseEnd :: State t s -> ParseResult n t s
parseEnd s = if null $ fst s
               then accept (parseTreeLeaf End) s
               else parseError $ snd s

parseNonTerminal :: (Eq t) => Grammar n t -> UpdateState t s -> n -> State t s -> ParseResult n t s
parseNonTerminal g u n s = parse (NonTerminal n) s
    where parse (Skip n)         = parseNonTerminalSymbol Skip n
          parse (NonTerminal n)  = parseNonTerminalSymbol NonTerminal n
          parse (Terminal t)     = parseTerminal u t
          parse End              = parseEnd
          parseNonTerminalSymbol symbol n s = fmap (map $ first $ ParseTree $ symbol n) $ applyAll (body $ rules g n) s
          apply st []     = Right [([], st)]
          apply st (x:xs) = case parse x st of
                                (Right ts) -> let (es, rs) = partitionEithers [fmap ((,) pt) $ apply s xs | (pt, s) <- ts]
                                              in if null rs
                                                   then Left $ concat es
                                                   else Right $ if shouldSkip x
                                                                  then concatMap snd rs
                                                                  else [(t:ts, s) | (t, cts) <- rs, (ts, s) <- cts]
                                (Left es) -> Left es
          applyAll bs st = let (es, cs) = partitionEithers $ map (apply st) bs
                           in if null cs
                                then Left $ concat es
                                else Right $ concat cs
          shouldSkip (Skip _) = True
          shouldSkip End      = True
          shouldSkip _        = False

parse :: (Eq t) => Grammar n t -> UpdateState t s -> s -> n -> [t] -> Either [s] [ParseTree n t]
parse g u p n w = case parseNonTerminal g u n (w, p) of
                      (Right ps) -> let (ws, rs) = partition (not . null . fst . snd) ps
                                    in if null rs
                                         then Left $ map (snd . snd) ws
                                         else Right $ map fst rs
                      (Left es) -> Left es