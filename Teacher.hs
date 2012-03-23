module Teacher where

import Automata

data Teacher sym = 
  Teacher {
    isMember :: [sym] -> Bool,
    isEquiv :: Automata [sym] sym -> Either () [sym]
    }