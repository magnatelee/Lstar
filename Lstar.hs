module Lstar where

import Automata
import Teacher

data LearnerState sym = LearnerState {
  r :: [[sym]], 
  e :: [[sym]],
  g :: [([sym], [sym], Bool)]
  }

class Finite sym where
  universe :: [sym]

findEquivState :: LearnerState sym -> [sym] -> sym -> [sym]
findEquivState lstate state sym = undefined

makeAutomata :: LearnerState sym -> Automata [sym] sym
makeAutomata lstate = Automata states [] finals trans
  where
    states = r lstate
    finals = undefined
    trans = undefined

lstar :: (Finite sym, Eq sym) => 
         Teacher sym -> Automata [sym] sym
lstar teacher = makeAutomata (lstar' initLearnerState)
  where
    lstar' lstate = 
      case isClosed lstate of
        Just state -> undefined
        Nothing -> 
          let conjecture = makeAutomata lstate in
          case isEquiv teacher conjecture of
            Left _ -> lstate
            Right cex -> lstar' (handleCex lstate cex)
    
    isClosed = undefined

    handleCex = undefined

    initLearnerState = LearnerState [[]] [[]] initTable
    singletons = map (\sym -> [sym]) universe
    initTable = 
      ([], [], isMember teacher []) :
      zip3 singletons (repeat [])
      (map (isMember teacher) singletons)