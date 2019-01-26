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

askForMore :: Backtracker -> ResolutionBacktrack -> IO ()
askForMore next b = let again = askForMore next b
                    in do
                         putStrLn "Should I try to find more solutions? [y/n]"
                         end <- isEOF
                         when (not end) $ do
                                            l <- getLine
                                            if null l
                                              then again
                                              else case l of
                                                       "y" -> tryFindMore
                                                       "Y" -> tryFindMore
                                                       "n" -> return ()
                                                       "N" -> return ()
                                                       c -> again
    where tryFindMore = result next $ next b

result :: Backtracker -> ResolutionResult -> IO ()
result next res = maybe noSolution solution res
    where noSolution = putStrLn "No solution."
          solution (sub, b) = do
                                printSolution sub
                                askForMore next b

runQuery :: QueryPipeline -> IO ()
runQuery pipeline = let runAgain = runQuery pipeline
                    in do
                         putStrLn "query ?-"
                         end <- isEOF
                         when (not end) $ do
                                            query <- getLine
                                            if null query
                                              then runAgain
                                              else case pipeline query of
                                                       (Left e) -> do
                                                                     printParseError e
                                                                     runAgain
                                                       (Right (res, next)) -> do
                                                                                result next res
                                                                                runAgain

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