module Teacher where

import Automata

data EQAnswer sym = EQUIV | CEX [sym]

data Teacher sym = 
  Teacher {
    isMember :: [sym] -> Bool,
    isEquiv :: Automata [sym] sym -> EQAnswer sym
    }