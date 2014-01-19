import Dict as D
import Dict (Dict)

iterate : Int -> (a -> a) -> a -> a
iterate n f x = if 0 == n then x
                else f (iterate (n-1) f x)

-- Invariant: probabilities sum to 1.
type Dist = Dict Int Float

-- TODO: permit duplicates.
-- normalizes
fromList : [(Int, Float)] -> Dist
fromList l = let total = sum <| map snd l
                 conj (xv,xp) d =
                     let pp = D.findWithDefault 0 xv d
                     in D.insert xv (pp + (xp / total)) d
             in l |> foldl conj D.empty

toList : Dist -> [(Int, Float)]
toList = D.toList

always : Int -> Dist
always x = fromList [(x,1)]

uniform : [Int] -> Dist
uniform xs = fromList <| map (\x -> (x,1)) xs

chance : Int -> Dist -> Float
chance x d = D.findWithDefault 0.0 x d

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


-- Dice
plus : Dist -> Dist -> Dist
plus x y = let app (xv,xp) (yv,yp) = (xv + yv, xp * yp)
           in toList x |> concatMap (\xe ->
              toList y |> map (app xe))
              |> fromList

d6 = uniform [1..6]
dFate = uniform [-1..1]

ndk : Int -> Dist -> Dist
ndk n d = case n of
            0 -> always 0
            1 -> d
            _ -> plus d (ndk (n-1) d)


-- Visualization


-- Program
main = plainText <| show (ndk 2 d6)
