module Main where

import Prolog.Engine
import Prolog.Types (Term(Var, Func))
import Data.List
import Data.Maybe
import Data.Either
import Control.Monad
import System.Environment (getArgs)
import System.IO (openFile, IOMode(ReadMode), hGetContents, hClose, isEOF)
                                    
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

askForMore :: [Solution] -> IO ()
askForMore solutions = do
                         putStrLn "Should I try to find more solutions? [y/n]"
                         end <- isEOF
                         unless end $ do
                                            l <- getLine
                                            if null l
                                              then askAgain
                                              else case l of
                                                       "y" -> tryFindMore
                                                       "Y" -> tryFindMore
                                                       "n" -> return ()
                                                       "N" -> return ()
                                                       c -> askAgain
    where tryFindMore = printSolutions solutions
          askAgain = askForMore solutions

printSolutions :: [Solution] -> IO ()
printSolutions solutions = if null solutions
                             then putStrLn "No solution."
                             else do
                                    printSolution $ head solutions
                                    askForMore $ tail solutions

runQuery :: QueryPipeline -> IO ()
runQuery pipeline = do
                      putStrLn "What do you want me to search for?"
                      end <- isEOF
                      query <- getLine
                      unless end $ processQuery query
    where processQuery query = do
                           if null query
                             then runAgain
                             else case pipeline query of
                                      (Left e) -> (printParseError e) >> runAgain
                                      (Right solutions) -> (printSolutions solutions) >> runAgain
          runAgain = runQuery pipeline

runEngine :: Text -> IO ()
runEngine content = either printParseError runQuery $ engine content
                                
main = do
         args <- getArgs
         if null args
           then putStrLn "Expecting file path to load knowledge base from."
           else if length args > 1
                  then putStrLn "Expecting exatcly one argument: file path to load knowledge base from."
                  else do
                         handle <- openFile (head args) ReadMode
                         content <- hGetContents handle
                         runEngine content
                         hClose handle