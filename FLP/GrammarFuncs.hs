module GrammarFuncs
    (dumpGrammar, simplifyGrammar, chomskyGrammar)
  where

import System.IO
import Control.Monad
import Data.List
import Data.List.Split
import Data.String
import Data.Char
import GrammarData

---------------------------------
--
-- Funkcie pre prepinac -i
--
---------------------------------

-- vypis terminaly/ neterminaly oddelene ciarkou
printElements :: [String] -> IO()
printElements all@(x:xs) = 
  if 1 == length all then do
    putStr $ x ++ "\n"
    return ()
    else do
      putStr $ x ++ ","
      printElements xs

-- vypis pravidla
printRules :: [Rule] -> IO()
printRules [] = return ()
printRules all@(rule:rules) = do
  putStr $ (from rule) ++ "->"
  putStrLn $ to rule
  printRules rules

dumpGrammar :: TGrammar -> IO()
dumpGrammar gr = do
    printElements $ nonterminals gr
    printElements $ terminals gr
    putStrLn $ start gr
    printRules $ rules gr



---------------------------------
--
-- Funkcie pre prepinac -1
--
---------------------------------

-- predikat jednoducheho pravidla
isNotSimpleRule :: Rule -> Bool
isNotSimpleRule r = if ((length $ to r) == 1) && (isUpper $ head $ to r) then False else True 

-- predikat pravidla zacinajuceho istym nonterminalom
startsWithNonterm :: TNonterminal -> Rule -> Bool
startsWithNonterm nonterm r = if (from r) == nonterm then True else False

-- odstran jednoduche pravidla zo vstupneho listu pravidiel
removeSimpleRules :: [Rule] -> [Rule]
removeSimpleRules [] = []
removeSimpleRules rules = filter isNotSimpleRule rules

-- segment nonterminals
segment :: TNonterminal -> [TNonterminal]
segment [] = []
segment [x] = [[x]]
segment (x:rest) = [x] : segment rest

-- najdi vsetky jednoduche pravidla pre neterminal n a vrat pravu cast (neterminaly) do ktorych vie prejst
findRightN :: TNonterminal -> [Rule] -> TNonterminal
findRightN n [] = []
findRightN n (r:rs) = if ((length $ to r) == 1) && (isUpper $ head $ to r) && ((head n) == (head $ from r)) then (head $ to r) : findRightN n rs else findRightN n rs

-- prvy krok algoritmu 4.5 - vytvorenie mnoziny Ni pre kazdy neterminal
createSet :: TNonterminal -> [Rule] -> Int -> [TNonterminal]
createSet nonterm rules index = if index < (length $ removeSimpleRules (rules)) && length nonterm > index then 
  createSet (nonterm ++ findRightN ([nonterm!!index]) rules) rules $ index+1
    else segment nonterm

-- najdi a premenuj vsetky pravidla na nonterm
--         jednoduche pravidla    vstupny nonterminal       mnozina N pre vstup. nonterm        nove pravidla
createRules :: [Rule]         ->     TNonterminal     ->          [TNonterminal]         ->       [[Rule]]
createRules rules nonterm [] = []
createRules rules nonterm (n:ns) = renameRules nonterm (filter (startsWithNonterm n) rules) : createRules rules nonterm ns

-- premenuj lavu stranu vsetkych pravidiel na nonterm
renameRules :: TNonterminal -> [Rule] -> [Rule]
renameRules nonterm rules = map (renameRule nonterm) rules

renameRule :: TNonterminal -> Rule -> Rule
renameRule nonterm rule = Rule nonterm (to rule)

-- pre kazdy neterminal zo sparsovanej gramatiky vytvor pravidla, premenuj, atd..
processNonterm :: [Rule] -> TNonterminal -> [[Rule]]
processNonterm rules nonterm = createRules (removeSimpleRules rules) nonterm (nub(createSet nonterm rules 0))

-- Simplify GRAMMAR
simplifyGrammar :: TGrammar -> TGrammar
simplifyGrammar grammar = TGrammar 
                            (nonterminals grammar)
                            (terminals grammar)
                            (start grammar)
                            (nub (concat $ concat ( map (processNonterm (rules grammar)) (nonterminals grammar) )))

---------------------------------
--
-- Funkcie pre prepinac -2
--
---------------------------------

-- rekurzivne vytvor pravidla z retazca (tvoreneho terminalmi alebo neterminalmi) povodne dlhsieho ako 2
createNewRules :: String -> [Rule]
createNewRules newNonterm
  -- koncime
  | (length newNonterm == 2) =
    if ( (isLower $ head newNonterm) && (isUpper $ last newNonterm) ) then
      -- napriklad -> aN
      [ Rule ("<" ++ newNonterm ++ ">") ( [head newNonterm]++ "'" ++ tail newNonterm) , Rule ([head newNonterm] ++ "'") ([head newNonterm]) ] else 
        if ((isLower $ last newNonterm) && (isUpper $ head newNonterm) ) then
          -- napriklad Na
          [ Rule ("<" ++ newNonterm ++ ">") ( [head newNonterm] ++ tail newNonterm ++ "'") , Rule ([last newNonterm] ++ "'") ([last newNonterm] ) ] else
            if( (isLower $ last newNonterm) && (isLower $ head newNonterm) ) then
              [ Rule ("<" ++ newNonterm ++ ">") ( [head newNonterm] ++ "'" ++ tail newNonterm ++ "'") , Rule ([last newNonterm] ++ "'") ([last newNonterm] ) , Rule ([head newNonterm] ++ "'") ([head newNonterm])  ] else
                [Rule ("<" ++ newNonterm ++ ">") ( [head newNonterm] ++ tail newNonterm)]
  -- vytvor pravidlo a zavolaj na druhu cast retazca znova
  | otherwise = 
    if (isLower $ head newNonterm) then Rule ("<" ++ newNonterm ++ ">") ( [head newNonterm] ++ "'<" ++ tail newNonterm ++ ">") : Rule ([head newNonterm] ++ "'") ([head newNonterm]) : createNewRules (tail newNonterm) else
      Rule ("<" ++ newNonterm ++ ">") ( [head newNonterm] ++ "<" ++ tail newNonterm ++ ">") : createNewRules (tail newNonterm)

-- narazili sme na pravidlo ktore treba rozdelovat
processComplexRule :: Rule -> [Rule]
processComplexRule rule =
  if (isLower $ head $ to rule) then
    -- napriklad N->tNN
    Rule (from rule) (([head $ to rule]) ++ "'<" ++ (tail $ to rule) ++ ">" ) : Rule (([head $ to rule]) ++ "'") (([head $ to rule])) : createNewRules (tail $ to rule) else 
      if (isLower $ head $ tail $ to rule) then
        -- Jedna sa o pravidlo napriklad ->NtN
        Rule (from rule) (([head $ to rule]) ++ "<" ++ (tail $ to rule) ++ ">" ) : createNewRules (tail $ to rule) else
          Rule (from rule) (([head $ to rule]) ++ "<" ++ (tail $ to rule) ++ ">" ) : createNewRules (tail $ to rule)

-- spracuj pravidlo, rozhodni ci sa musi spracovavat alebo nie (NN, t)
processRule :: Rule -> [Rule]
processRule rule = if ( ((length $ to rule) == 2) && (isUpper $ head $ to rule) && (isUpper $ last $ to rule) ) || ( ((length $ to rule) == 1) && (isLower $ head $ to rule) ) then
  -- Jedna sa o pravidlo ->NN alebo ->t, takze len vrat pravidlo v poradi
  [rule] else if ( ((length $ to rule) == 2) && (isLower $ head $ to rule) && (isLower $ last $ to rule) ) then
    [Rule (from rule) (([head $ to rule]) ++ "'" ++ ([last $ to rule]) ++ "'")] else
      processComplexRule rule

-- K mnozine neterminalov pridaj novo vytvorene neterminaly a vloz ich spolu do listu stringov
printNonterminals :: [Rule] -> [String]
printNonterminals [] = []
printNonterminals (rule:rules) = from rule : printNonterminals rules

-- Prevod gramatiky do Chomskeho normalnej formy
chomskyGrammar :: TGrammar -> TGrammar
chomskyGrammar grammar = TGrammar 
                            (nub $ (nonterminals grammar) ++ (nub $ printNonterminals (nub $ concat $ map (processRule) (rules grammar) ) ))
                            (terminals grammar)
                            (start grammar)
                            (nub $ concat $ map (processRule) (rules grammar) )
