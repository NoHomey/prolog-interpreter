module Main where

import qualified Data.KeyedCollection as KC
import qualified Prolog.Parse.Grammar as G
import qualified Prolog.OptimizeForUnification as OU
import qualified Prolog.PipelineFromTextToOptimizedForUnification as PTOU
import qualified Prolog.Resolution as Res
import qualified Prolog.Unification as U
import qualified Prolog.Parse.Result as PR
import qualified Prolog.Types as T
import Data.AssocListTrie
import Data.DTrie
import Data.Either
import Data.Maybe
import Data.List
import Data.Bifunctor

type Identifier = Int

type NextIdentifier = Identifier -> Identifier

type RenameCollection = AssocListTrie G.Terminal Identifier

type RenameInfo = (Identifier, RenameCollection)

type Rules = OU.Rules Identifier Identifier Identifier

type KnowledgeBase = DTrie Identifier Rules

type QueryRenameInfo = (RenameInfo, RenameInfo, RenameInfo)

type Query = OU.Query Identifier Identifier Identifier

type Substitution = U.Substitution Identifier (Identifier, Identifier)

type ResolutionPath = Res.ResolutionPath Identifier Identifier Identifier

type Env = (NextIdentifier, Identifier, KnowledgeBase)

type Backtrack = (Env, ResolutionPath)

type ResolutionResult = Maybe (Substitution, Backtrack)

nextIdentifier :: NextIdentifier
nextIdentifier = (+1)

initialIdentifier :: Identifier
initialIdentifier = 1

initialRenameInfo :: RenameInfo
initialRenameInfo = (initialIdentifier, KC.empty)

pipelineForKB :: [G.Terminal] -> Either PTOU.ParseError (KnowledgeBase, (RenameInfo, RenameInfo))
pipelineForKB = PTOU.pipelineForKnowledgeBase nextIdentifier nextIdentifier nextIdentifier (initialRenameInfo , initialRenameInfo) initialRenameInfo 

pipelineForQuery :: QueryRenameInfo -> [G.Terminal] -> Either PTOU.ParseError (Query, QueryRenameInfo)
pipelineForQuery = PTOU.pipelineForQuery nextIdentifier nextIdentifier nextIdentifier

resolve :: KnowledgeBase -> Query -> ResolutionResult
resolve = Res.resolve nextIdentifier initialIdentifier

next :: Backtrack -> ResolutionResult
next = Res.next

unmapVar :: RenameCollection -> (Identifier, Identifier) -> PR.Identifier
unmapVar varsC = varToIdentifier $ KC.assoc varsC
    where varToIdentifier mapped v@(rid, x) = if rid == initialIdentifier
                                                then fst $ fromJust $ find ((x ==). snd) mapped
                                                else "$" ++ (show v) 

unmapSym :: RenameCollection -> OU.ArityIdentifier Identifier -> PR.Identifier
unmapSym symsC = symToIdentifier $ KC.assoc symsC
    where symToIdentifier mapped ai = fst $ fromJust $ find (((OU.identifier ai) ==). snd) mapped

showTermWithIdentifiers :: PR.Term -> String
showTermWithIdentifiers (T.Var x)     = x
showTermWithIdentifiers (T.Func f ps) = if null ps
                                         then f
                                         else f ++ "(" ++ (concat $ intersperse ", " $ map showTermWithIdentifiers ps) ++ ")" 

printSolution :: RenameCollection -> RenameCollection -> Substitution -> IO()
printSolution symsC varsC s = let idVar = unmapVar varsC
                                  idSym = unmapSym symsC
                                  idTerm = bimap idSym idVar
                                  identified = map (bimap idVar idTerm) s
                              in do
                                    print "Found solution."
                                    print "answers:"
                                    if null identified
                                      then print "No answers."
                                      else mapM_ (\(v, t) -> print $ v ++ " = " ++ (showTermWithIdentifiers t)) identified

askForMore :: RenameCollection -> RenameCollection -> Backtrack -> IO()
askForMore symsC varsC b = do
                             print "Should I try to find more solutions? [y/n]"
                             let askAgain = askForMore symsC varsC b
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
    where tryFindMore = result symsC varsC $ next b

result :: RenameCollection -> RenameCollection -> ResolutionResult -> IO()
result symsC varsC res = maybe (print "No solution.") solution res
    where solution (sub, b) = do
                                printSolution symsC varsC sub
                                askForMore symsC varsC b

printInfo :: (KnowledgeBase, Query, QueryRenameInfo) -> IO()
printInfo (kb, q, ((p, predsC), (s, symsC), (v, varsC))) = result symsC varsC $ resolve kb q

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

test :: ([G.Terminal], [G.Terminal]) -> Either PTOU.ParseError (KnowledgeBase, Query, QueryRenameInfo)
test (content, prompt) = do
                           (kb, (preds, syms)) <- pipelineForKB content
                           (query, info) <- pipelineForQuery (preds, syms, initialRenameInfo) prompt
                           return (kb, query, info)
                                
main = mapM_ (either print printInfo . test) $ zip [c1, c2, c3, c4, c5] [p1, p2, p3, p4, p5]