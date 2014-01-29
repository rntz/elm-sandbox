-- Moore machines with a visible state type.
-- See also Moore.elm
module OpenMoore where

import List

import Automaton
import Automaton (Automaton)

import AutomatonUtil
import Moore
import Moore (Moore)

-- State machine
type SM s i o = { view: s -> o
                , update: i -> s -> s
                , state: s }

create : s -> (s -> o) -> (i -> s -> s) -> SM s i o
create init view update = { state = init, view = view, update = update }

toMoore : SM s i o -> Moore i o
toMoore sm = Moore.create sm.state sm.view sm.update

fromMoore : Moore i o -> SM (Moore i o) i o
fromMoore hsm = create hsm Moore.get Moore.step

get : SM s i o -> o
get sm = sm.view sm.state

step : i -> SM s i o -> SM s i o
step i sm = {sm | state <- sm.update i sm.state}

run : SM s i o -> Signal i -> Signal o
run sm = Moore.run (hide sm)

apply : (s -> s) -> SM s i o -> SM s i o
apply f sm = {sm | state <- f sm.state }

-- Transformers and combinators on state machines
inMap : (a -> b) -> SM s b o -> SM s a o
inMap f sm = {sm | update <- \x -> sm.update (f x) }

outMap : (a -> b) -> SM s i a -> SM s i b
outMap f sm = {sm | view <- f . sm.view }

-- f,g must form a bijection.
stateMap : (a -> b) -> (b -> a) -> SM a i o -> SM b i o
stateMap f g sm = {sm | state <- f sm.state
                      , view <- sm.view . g
                      , update <- \i b -> f (sm.update i (g b)) }

combine : [SM s i o] -> SM [s] i [o]
combine sms =
    { state = map .state sms
    , view = List.zipWith .view sms
    , update i = List.zipWith (\sm -> sm.update i) sms }
