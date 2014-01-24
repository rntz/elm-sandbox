module AutomatonUtil where

import open Automaton

map : (b -> c) -> Automaton a b -> Automaton a c
map f a = a `andThen` pure f

stateView : s -> (s -> o) -> (i -> s -> s) -> Automaton i o
stateView init view update = map view <| state init update
