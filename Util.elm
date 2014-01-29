module Util where

import open Either

import Automaton
import Automaton (Automaton)

-- General utilities
fromMaybe : a -> Maybe a -> a
fromMaybe x = maybe x id

tau : Float
tau = 2 * pi

lg = logBase 2

iterate : Int -> (a -> a) -> a -> a
iterate n f x = if 0 == n then x
                else f (iterate (n-1) f x)

enumerate : [a] -> [(Int,a)]
enumerate l = zip [0 .. length l - 1] l

mapi : (Int -> a -> b) -> [a] -> [b]
mapi f l = map (uncurry f) (enumerate l)

takeWhile : (a -> Bool) -> [a] -> [a]
takeWhile p l =
    case l of [] -> []
              x::xs -> if p x then x :: takeWhile p xs else []

index : (a -> Bool) -> [a] -> Maybe Int
index p =
    let find i l = case l of [] -> Nothing
                             x::xs -> if p x then Just i
                                      else find (i+1) xs
    in find 0

replicate : Int -> a -> [a]
replicate n e = if 0 == n then []
                else e :: replicate (n-1) e

-- Signals and Automata
infixl 0 ~>
(~>) : Signal a -> (a -> b) -> Signal b
x ~> f = lift f x

mergeEither : Signal a -> Signal b -> Signal (Either a b)
mergeEither l r = merge (Left <~ l) (Right <~ r)

-- Elements
column : Int -> [Element] -> Element
column sp elts = flow down <| intersperse (spacer 1 sp) elts

row : Int -> [Element] -> Element
row sp elts = flow right <| intersperse (spacer sp 1) elts

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
