module Automata where

import Data.List

type Trans state sym = [(state, sym, state)]

data Automata state sym = Automata {
  states :: [state],
  initial :: state, 
  finals :: [state], 
  trans :: Trans state sym
  }

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a
snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b
thd3 :: (a, b, c) -> c
thd3 (a, b, c) = c

next :: (Eq state, Eq sym) => 
        Automata state sym -> state -> sym -> Maybe state
next auto from sym = 
  fmap thd3 $ 
  find (\(from', sym', _) ->  from' == from && sym' == sym) delta 
  where
    delta = trans auto

isInitial :: Eq state => Automata state sym -> state -> Bool
isInitial auto s = initial auto == s

isFinal :: Eq state => Automata state sym -> state -> Bool
isFinal auto s = s `elem` finals auto

