module Dist where

import Dict (Dict)
import Dict
import List

-- Invariant: probabilities sum to 1.
type Dist = Dict Int Float

-- Normalizes & deduplicates.
fromList : [(Int, Float)] -> Dist
fromList l = let total = List.sum <| map snd l
                 conj (xv,xp) d =
                     let pp = Dict.findWithDefault 0 xv d
                     in Dict.insert xv (pp + (xp / total)) d
             in l |> foldl conj Dict.empty

toList : Dist -> [(Int, Float)]
toList = Dict.toList

always : Int -> Dist
always x = fromList [(x,1)]

uniform : [Int] -> Dist
uniform xs = fromList <| map (\x -> (x,1)) xs

chance : Int -> Dist -> Float
chance x d = Dict.findWithDefault 0.0 x d

-- Dice
plus : Dist -> Dist -> Dist
plus x y = let app (xv,xp) (yv,yp) = (xv + yv, xp * yp)
           in toList x |> concatMap (\xe ->
              toList y |> map (app xe))
              |> fromList

sum : [Dist] -> Dist
sum = foldr plus (always 0)

outMap : (Int -> Int) -> Dist -> Dist
outMap f d = toList d |> map (\(x,p) -> (f x, p)) |> fromList

negate = outMap (\x -> -x)

{-
-- {Functor, Applicative, Monoid} Dist
infixl 4 <$>
infixl 4 <*>
infixl 1 >>=

pure = always
f <$> l = map (\(a,p) -> (f a, p)) l
fd <*> ad = let app (fv,fp) (av,ap) = (fv av, fp * ap)
            in fd |> concatMap (\f -> map (app f) ad)
k >>= f = k |> concatMap (\(res, resP) ->
          f res |> map (\(x, xP) -> (x, xP * resP)))
-}
