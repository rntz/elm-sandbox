module Util where

import Automaton
import Automaton (Automaton)

-- Utilities
infixl 0 ~>
(~>) : Signal a -> (a -> b) -> Signal b
x ~> f = lift f x

stateView : s -> (s -> o) -> (i -> s -> s) -> Automaton i o
stateView init view update =
    Automaton.hiddenState init
             (\event state -> let newState = update event state
                              in (newState, view newState))

stack : Int -> [Element] -> Element
stack sp elts = flow down <| intersperse (spacer 1 sp) elts

replicate : Int -> a -> [a]
replicate n e = if 0 == n then []
                else e :: replicate (n-1) e

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
