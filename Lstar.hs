module Lstar where

import Data.List
import Data.Maybe
  
import Automata
import Teacher

data Bin = O | I deriving (Show, Eq)

data LearnerState sym = LearnerState {
  getR  :: [[sym]], 
  getE  :: [[sym]],
  getG  :: [([sym], [Bin])],
  getGS :: [([sym], sym, [Bin])]
  }

class Finite sym where
  universe :: [sym]

findEquivState :: LearnerState sym -> [sym] -> sym -> [sym]
findEquivState lstate state sym = undefined

-- g0 = LearnerState [[], [A]] 
--      [            [] ]
--      [(  [] ,    [ I ])]
--      [(  [] , A, [ O ]), 
--       (  [] , B, [ O ])]

-- g1 = LearnerState [[], [A]] 
--      [            [] ]
--      [(  [] ,    [ I ]),
--       ( [A] ,    [ O ])]
--      [(  [] , A, [ O ]), 
--       (  [] , B, [ O ]), 
--       ( [A] , A, [ I ]),
--       ( [A] , B, [ O ])]

-- g2 = LearnerState [[], [A], [B]] 
--      [            [B], [] ]
--      [(  [] ,   [  O ,  I ]), 
--       ( [A] ,   [  O ,  O ]), 
--       ( [B],    [  I ,  O ])]
--      [(  [], A, [  O ,  O ]), 
--       (  [], B, [  I ,  O ]), 
--       ( [A], A, [  O ,  I ]), 
--       ( [A], B, [  O ,  O ]), 
--       ( [B], A, [  O ,  O ]), 
--       ( [B], B, [  O ,  I ])]

instance Show sym => Show (LearnerState sym) where
  show lstate =
    "[R] "  ++ show (getR  lstate) ++ "\n" ++
    "[E] "  ++ show (getE  lstate) ++ "\n" ++
    "[G] "  ++ show (getG  lstate) ++ "\n" ++
    "[GS] " ++ show (getGS lstate)

boolToBin :: Bool -> Bin
boolToBin True  = I
boolToBin False = O

binToBool :: Bin -> Bool
binToBool I = True
binToBool O = False

lift :: a -> [a]
lift x = [x]

data AnswerOfIsClosed sym = HasNext [sym] | NewNext [sym]

instance Show sym => Show (AnswerOfIsClosed sym) where
  show (HasNext state) = "[NEXT] " ++ show state
  show (NewNext state) = "[NEW] " ++ show state

fromHasNext (HasNext r) = r

isNew (NewNext _) = True
isNew _ = False

isClosed :: [([sym], [Bin])] -> ([sym], sym, [Bin]) -> AnswerOfIsClosed sym
isClosed g (r, a, bins) = 
  case find (\(_, bins') -> bins == bins') g of
    Just (r', _) -> HasNext r'
    Nothing -> NewNext (r ++ [a])

isClosedLstate :: Eq sym => LearnerState sym -> AnswerOfIsClosed sym
isClosedLstate lstate = 
  case find isNew (map (isClosed (getG lstate)) $ getGS lstate) of
    Just new -> new
    _ -> HasNext []

handleNewNext :: (Eq sym, Finite sym) => 
                 ([sym] -> Bin) -> LearnerState sym -> [sym] -> LearnerState sym
handleNewNext mem lstate newState = 
  LearnerState (newState : r) e g' gs'
  where
    r = getR lstate
    e = getE lstate
    g = getG lstate
    g' = g ++ [(newState, map (mem . (newState++)) e)]
    gs = getGS lstate
    nextStep sym = (newState, sym, map (mem . (newState++) . (sym:)) e)
    gs' = gs ++ map nextStep universe

handleCex :: (Eq sym, Finite sym) =>
             ([sym] -> Bin) -> LearnerState sym -> [sym] -> LearnerState sym
handleCex mem lstate newE =
  LearnerState r (newE : e) g' gs'
  where
    r = getR lstate
    e = getE lstate
    g = getG lstate
    g' = map (\(state, bins) -> (state, mem (state ++ newE):bins)) g
    gs = getGS lstate
    gs' = map (\(state, sym, bins) -> (state, sym, mem (state ++ sym:newE):bins)) gs

makeAutomata :: Eq sym => LearnerState sym -> Automata [sym] sym
makeAutomata lstate = Automata states [] finals (Trans trans)
  where
    states = getR lstate
    finals = map fst $ filter (binToBool . last . snd) $ getG lstate
    findNext s@(r, a, bins) = 
      (r, a, fromHasNext . isClosed (getG lstate) $ s)
    trans = map findNext $ getGS lstate

lstar :: (Eq sym, Finite sym) => 
         Teacher sym -> Automata [sym] sym
lstar teacher = makeAutomata (lstar' initLearnerState)
  where
    mem = boolToBin . isMember teacher
    equiv = isEquiv teacher

    lstar' lstate = 
      case isClosedLstate lstate of
        NewNext state -> lstar' (handleNewNext mem lstate state)
        _ ->
          let conjecture = makeAutomata lstate in
          case equiv conjecture of
            EQUIV -> lstate
            CEX cex -> lstar' (handleCex mem lstate cex)

    initLearnerState = LearnerState [[]] [[]] initg initgs
    initg = [([], [boolToBin $ isMember teacher []])]
    initgs =
      zip3 (repeat []) universe
      (map (lift . boolToBin .  isMember teacher . lift) universe)