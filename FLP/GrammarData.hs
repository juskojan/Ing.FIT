module GrammarData where

import Data.List


-- Type for specifying action
data CAction
    = Dump
    | Simulate
    | Simplify
  deriving (Show, Eq)

-- Type for specifying argument combination
data Config = Config
    { action :: CAction
    , location :: FilePath
    }
  deriving (Show)


type TNonterminal = String
type TTerminal    = String
type TExpanze     = String

data TGrammar = TGrammar
    { nonterminals :: [TNonterminal]
    , terminals    :: [TTerminal]
    , start        :: TNonterminal
    , rules        :: [Rule]
    }
    deriving (Show)

data Rule = Rule
    { from :: TNonterminal
    , to   :: TExpanze
    }
    deriving (Eq, Show)