module Main where
  
import Automata
import Lstar
import Teacher

data Sym = A | B deriving (Show, Eq)

instance Finite Sym where
  universe = [A, B]

auto = Automata [[], [A], [B], [A,B]] [] [[]] $ Trans
       [(   [], A,   [A]),
        (   [], B,   [B]),
        (  [A], A,    []),
        (  [A], B,  [A,B]),
        (  [B], A,  [A,B]),
        (  [B], B,    []),
        ([A,B], A,   [B]),
        ([A,B], B,   [A])]

member :: [Sym] -> Bool
member str = member' (initial auto) str where
  member' state [] = isFinal auto state
  member' state (sym:str) = 
    case next auto state sym of
      Just state' -> member' state' str

equiv :: Automata [Sym] Sym -> EQAnswer Sym
equiv a = 
  if length (states a) == 2
  then CEX [B]
  else 
    if length (states a) == 3 
    then CEX [A]
    else EQUIV
  
main = do  
  putStrLn $ show $ lstar (Teacher member equiv)