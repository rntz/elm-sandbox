module MooreList where

import List

import Automaton (Automaton)
import Automaton

import AutomatonUtil
import DynList (DynList)
import DynList as DL
import Moore
import Moore (Moore)

type MooreList i o = DynList (Moore i o)

empty : MooreList i o
empty = DL.empty

get : MooreList i o -> [o]
get = List.map snd . DL.get

pushNew : (Int -> (Automaton i o, o)) -> MooreList i o -> MooreList i o
pushNew = DL.pushNew

push : (Automaton i o, o) -> MooreList i o -> (Int, MooreList i o)
push = DL.push

remove : Int -> MooreList i o -> MooreList i o
remove = DL.remove

update : i -> MooreList i o -> MooreList i o
update i ml = DL.map (Automaton.step i . fst) ml

step : i -> MooreList i o -> (MooreList i o, [o])
step i ml = let newAL = update i ml in (newAL, get newAL)

toMoore : MooreList i o -> Moore i [o]
toMoore ml = Moore.create ml get update

run : MooreList i o -> Signal i -> Signal [o]
run = Moore.run . toMoore
