---------------------------------
-- FLP projekt 2017L
-- Jan Jusko
-- xjusko00@stud.fit.vutbr.cz
--
-- FIT VUT Brno
---------------------------------

module Main (main) where
-- Imported 3rd party libraries
import System.IO
import System.Environment
import Data.Char
-- Imported own custom types, functions and parser
import GrammarData
import GrammarParser
import GrammarFuncs

-- Main
main :: IO ()
main = do
    args <- getArgs
    let arg_structure = procArgs args
    if (location arg_structure) == "stdin" then do 
    	contents <- getContents
    	handleGrammar (action arg_structure) contents
       else do
       	handle <- openFile (location arg_structure) ReadMode
       	contents <- hGetContents handle
       	handleGrammar (action arg_structure) contents


-- Parse grammar into structure and perform operation
handleGrammar :: CAction -> String -> IO()
handleGrammar action input = do
	let parsedGrammar = parseGrammar input
	case action of
		Dump -> dumpGrammar parsedGrammar
		Simplify -> dumpGrammar $ simplifyGrammar parsedGrammar
		Simulate -> dumpGrammar $ chomskyGrammar $ simplifyGrammar parsedGrammar

-- Processing command line arguments
procArgs :: [String] -> Config
procArgs [] = error "Program expects two arguments!"
procArgs [x]
    | x == "-i" = Config Dump "stdin"
    | x == "-1" = Config Simplify "stdin"
    | x == "-2" = Config Simulate "stdin" 
    | otherwise = error "Wrong combination of arguments."
procArgs [x,y]
    | x == "-i" = Config Dump y
    | x == "-1" = Config Simplify y
    | x == "-2" = Config Simulate y
    | otherwise = error "Wrong combination of arguments."
procArgs _ = error "Too many arguments!"
