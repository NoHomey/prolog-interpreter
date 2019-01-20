module Main where

import qualified Prolog.Parse.ParseTree as PT
import qualified Prolog.Parse.Result as PR
import qualified Prolog.Parse.Grammar as PG
import Data.Bifunctor

content = "nat(zero). nat(X) :- nat(Y), is(X, succ(Y)). is(X, X)."

prompt = "nat(Y), is(X, succ(Y))."

prog = bimap maximum (PR.rules . head) $ PT.parse PG.Start content

query = bimap maximum (PR.query . head) $ PT.parse PG.Query prompt

main = do
         print prog
         print query