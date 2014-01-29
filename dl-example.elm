import Graphics.Input as Input
import open Either

import Automaton
import Automaton (Automaton)

import open Util
import AutomatonUtil
import DynList as DL
import DynList (DynList)
import MooreList as ML
import MooreList (MooreList)

import Moore
import Moore (Moore)

data ListEvent = Nop | Create | Destroy Int
listButtons = Input.buttons Nop
newButton = listButtons.button Create "New element!"

eltList : (Int -> Moore i o) -> Moore (Either ListEvent i) [o]
eltList mkElt =
    let update evt ml = case evt of
                          Nop -> ml
                          Create -> ML.pushNew mkElt ml
                          Destroy i -> ML.remove i ml
    in Moore.create ML.empty ML.get (either update ML.update)

-- My elements
type EltEvent = Maybe Int
eltButtons = Input.buttons Nothing

newElement : Int -> Moore (Maybe Int) Element
newElement ident =
    let destroyBtn = listButtons.button (Destroy ident) "Destroy me!"
        incBtn = eltButtons.button (Just ident) "Push me!"
        view st = row 5 [asText ident, destroyBtn, incBtn, asText st]
        update e st =
            case e of Just j -> if ident == j then st + 1 else st
                      _ -> st
    in Moore.create 0 view update

main =
    let elts = eltList newElement
        display es = column 10 (newButton :: es)
        evts = mergeEither listButtons.events eltButtons.events
    in display <~ Moore.run elts evts
