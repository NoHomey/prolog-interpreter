module Main where

import Prolog.Engine
import Prolog.Types (Term(Var, Func))
import Data.List
import Data.Maybe
                                    
printSolution :: Solution -> IO ()
printSolution s = do
                    putStrLn "Found solution."
                    putStrLn "answers:"
                    if null s
                      then putStrLn "No answers."
                      else mapM_ (\(v, t) -> putStrLn $ v ++ " = " ++ (termWithIdentifiers t)) s
    where termWithIdentifiers (Var x)     = x
          termWithIdentifiers (Func f ps) = if null ps
                                              then f
                                              else f ++ "(" ++ (concat $ intersperse ", " $ map termWithIdentifiers ps) ++ ")" 

printParseError :: ParseError -> IO ()
printParseError (from, (line, col)) = do
                                        putStrLn $ "Parsing error at: line " ++ (show line) ++ " column " ++ (show col) ++ "."
                                        putStrLn $ "While parsing: " ++ (strFrom from) ++ "."
    where strFrom Rules = "knowledge base content"
          strFrom Query = "query"                           

askForMore :: Backtracker -> ResolutionBacktrack -> IO ()
askForMore next b = do
                      putStrLn "Should I try to find more solutions? [y/n]"
                      let askAgain = askForMore next b
                      l <- getLine
                      if null l
                        then askAgain
                        else let c = head l
                             in case c of
                                    'y' -> tryFindMore
                                    'Y' -> tryFindMore
                                    'n' -> return ()
                                    'N' -> return ()
                                    c -> askAgain
    where tryFindMore = printResult next $ next b

printResult :: Backtracker -> ResolutionResult -> IO ()
printResult next res = maybe (putStrLn "No solution.") solution res
    where solution (sub, b) = do
                                printSolution sub
                                askForMore next b

loadContent :: (Text, Text) -> IO()
loadContent (c, p) = case engine c of
                         (Left e) -> printParseError e
                         (Right pipeLineForQuery) -> case pipeLineForQuery p of
                                                         (Left e) -> printParseError e
                                                         (Right (res, next)) -> printResult next res

c1 = "f(a). f(b). g(a). g(b). h(b). k(X) :- f(X), g(X), h(X)."
c2 = "nat(zero). nat(X) :- nat(Y), is(X, succ(Y)). is(X, X)."
c3 = "l(v, m). l(h, m). j(X, Y) :- l(X, Z), l(Y, Z)."
c4 = "m(X, l(X, T)). m(X, l(H, T)) :- m(X, T)."
c5 = "f(j, b). f(j, s). f(s, c). f(c, a). f(s, t). g(X, Z) :- f(X, Y), f(Y, Z)."

p1 = "k(Y)."
p2 = "nat(X)."
p3 = "j(X, Y)."
p4 = "m(Z, l(a, l(b, null))), m(Z, l(b, l(c, null)))."
p5 = "g(G, X)."
                                
main = mapM_ loadContent $ zip [c1, c2, c3, c4, c5] [p1, p2, p3, p4, p5]