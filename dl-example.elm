import Graphics.Input as Input
import Automaton
import Automaton (Automaton, (>>>), (<<<))

import DynList (DynList)
import DynList as DL

-- Utilities
infixl 0 ~>
(~>) : Signal a -> (a -> b) -> Signal b
x ~> f = lift f x

stack : Int -> [Element] -> Element
stack sp elts = flow down <| intersperse (spacer 1 sp) elts

replicate : Int -> a -> [a]
replicate n e = if 0 == n then []
                else e :: replicate (n-1) e

listInsert : Int -> a -> [a] -> [a]
listInsert n e l = if n == 0 then e :: l
                   else case l of
                          [] -> [e]
                          x::xs -> x :: listInsert (n-1) e xs

listRemove : Int -> [a] -> [a]
listRemove n l = case l of
                   [] -> []
                   x::xs -> if n == 0 then xs
                            else x :: listRemove (n-1) xs

stateView : s -> (s -> o) -> (i -> s -> s) -> Automaton i o
stateView init view update =
    Automaton.hiddenState init
             (\event state -> let newState = update event state
                              in (newState, view newState))

-- Labeling elements
-- widthFactor: Roughly, "how many characters maximum"
-- heightFactor: Fraction of element height text should be
type LabelConfig = { widthFactor: Float
                   , heightFactor: Float }

defaultLabelCfg = { widthFactor = 5.0
                  , heightFactor = 0.28 }

label : LabelConfig -> Text -> Element -> Element
label cfg txt elem =
  let txtH = cfg.heightFactor * toFloat (heightOf elem)
      lblW = floor <| txtH * cfg.widthFactor
      spc = floor <| txtH * 0.8
      lbl = txt |> Text.height txtH |> righted
            |> container lblW (heightOf elem) midRight
  in flow right [ lbl, spacer spc 1, elem ]

labelS s elem = label defaultLabelCfg (toText s) elem

-- A control for a list whose elements can be dynamically added & removed by the
-- user
-- dynamicList : (() -> Signal Element) -> Signal Element
-- dynamicList new = undefined

-- dynlist : (() -> (Signal Bool, Signal Element)

-- dynamic : a -> Automaton (a -> a) a
-- dynamic init = Automaton.state init (\f state -> f state)

-- type DynList a = { nextUid: Int, list: [(Int, a)] }

-- dlEmpty : DynList a
-- dlEmpty = { nextUid = 0, list = [] }

-- dlGet : DynList a -> [a]
-- dlGet dl = map snd dl.list

-- dlPushNew : (Int -> a) -> DynList a -> DynList a
-- dlPushNew f dl = { dl | nextUid <- dl.nextUid + 1
--                       , list <- (dl.nextUid, f dl.nextUid) :: dl.list }

-- dlPush : a -> DynList a -> (Int, DynList a)
-- dlPush x dl = (dl.nextUid,
--                { dl | nextUid <- dl.nextUid + 1
--                     , list <- (dl.nextUid, x) :: dl.list })

-- dlRemove : Int -> DynList a -> DynList a
-- dlRemove i dl = { dl | list <- filter (\(j,_) -> i /= j) dl.list }


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
