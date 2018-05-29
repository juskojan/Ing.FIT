module GrammarParser
    (parseGrammar)
  where

import Text.ParserCombinators.ReadP
import TuringData

-- replace arrow in rules with two commas for better parsing
replaceArrow :: String -> String
replaceArrow [] = []
replaceArrow (x:xs) = 
     if x == '-' || x == '>'
     then ',' : replaceArrow xs 
     else x : replaceArrow xs


parseGrammar :: String -> TGrammar
parseGrammar s = case readP_to_S gParser (replaceArrow s) of
    [(gr, _)] -> gr
    _ -> error "Cannot parse this input!"

comma :: ReadP Char
comma = char ','

newLine :: ReadP Char
newLine = char '\n'

parseState = many1 $ satisfy (/= ',')

parseStates = sepBy1 parseState comma

parseTo = many1 $ satisfy (/= '\n')

parseRules = many1 $ do
  r <- parseRule
  newLine
  return r
  where
    parseRule = do
      from <- get
      comma
      comma
      to <- parseTo
      return $ Rule [from] to

gParser :: ReadP TGrammar
gParser = do
  nonterms <- parseStates
  newLine
  terms <- parseStates
  newLine
  start <- parseState
  newLine
  rules <- parseRules
  eof
  return $ TGrammar nonterms terms start rules