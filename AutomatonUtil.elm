module AutomatonUtil where

import open Automaton

map : (b -> c) -> Automaton a b -> Automaton a c
map f a = a `andThen` pure f

inMap : (a -> b) -> Automaton b c -> Automaton a c
inMap f a = pure f `andThen` a

stateView : s -> (s -> o) -> (i -> s -> s) -> Automaton i o
stateView init view update = map view <| state init update
