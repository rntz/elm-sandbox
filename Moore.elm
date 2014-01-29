-- State machines (not necessarily finite) with a hidden/abstract state type
-- Automatons are like Mealy machines; these are like Moore machines.
-- See also OpenMoore.elm
module Moore where

import List

import Automaton
import Automaton (Automaton)

import AutomatonUtil

type Moore i o = (Automaton i o, o)

create : s -> (s -> o) -> (i -> s -> s) -> Moore i o
create init view update = (AutomatonUtil.stateView init view update,
                           view init)

get : Moore i o -> o
get = snd

step : i -> Moore i o -> Moore i o
step i (a,_) = Automaton.step i a

run : Moore i o -> Signal i -> Signal o
run (a,o) = Automaton.run a o

-- Transformers & combinators
inMap : (a -> b) -> Moore b o -> Moore a o
inMap f (a,o) = (AutomatonUtil.inMap f a, o)

outMap : (a -> b) -> Moore i a -> Moore i b
outMap f (a,o) = (AutomatonUtil.map f a, f o)

combine : [Moore i o] -> Moore i [o]
combine ms = (Automaton.combine (map fst ms), map snd ms)
