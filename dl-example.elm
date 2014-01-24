import Graphics.Input as Input
import Automaton
import Automaton (Automaton, (>>>), (<<<))

import open Util

import DynList (DynList)
import DynList as DL

data ButtonEvent = Nop | Create | Destroy Int
buttons = Input.buttons Nop
newButton = buttons.button Create "New element!"

eltList : (Int -> Element) -> Automaton ButtonEvent [Element]
eltList newElt =
    let view = DL.get
        update evt dl = case evt of
                          Nop -> dl
                          Create -> DL.pushNew newElt dl
                          Destroy i -> DL.remove i dl
    in stateView DL.empty view update

main =
    let newElement : Int -> Element
        newElement i = buttons.button (Destroy i) ("Destroy me! " ++ show i)
        elts = Automaton.run (eltList newElement) [] buttons.events
        display es = stack 10 (newButton :: es)
    in display <~ elts
