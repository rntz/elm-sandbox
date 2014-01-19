import Dict as D
import Dict (Dict)
import Transform2D (Transform2D, matrix, identity)

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
            _ -> d `plus` ndk (n-1) d

fate = ndk 4 dFate
bias = always -7 `plus` ndk 2 d6


-- Visualization
-- I assume D.keys returns keys in sorted order.
bounds : Dist -> (Int, Int)
bounds d = let k = D.keys d in (head k, last k)

-- weird 2^i = 1/(1+i)
--weird : Float -> Float
--weird x = 1/(1 + lg x)

-- weird _ _ 0 = 1
-- weird b r r = b
-- weird b r (2r) = b^2
-- weird b r (3r) = b^3
weird : Float -> Float -> Float -> Float
weird b r x = b^(x/r)

-- zero is black, farther away gets white
intColor : Int -> Color
intColor i =
  let j = toFloat (abs i)
      hue = turns (j / 7.0)
      sat = 1 - weird (1/2) 1 j
      val = weird (4/5) 6 j
  in hsv hue sat val

scaleXY : Float -> Float -> Transform2D
scaleXY x y = matrix x 0 0 y 0 0
scaleX x = scaleXY x 1.0
scaleY y = scaleXY 1.0 y

graphBar : [(Int,Float)] -> Form
graphBar l =
  let foo (v,p) rest =
        group [ rect p 1 |> filled (intColor v) |> move (p/2, 0)
              , rest |> move (p, 0) ]
  in foldr foo (group []) l

graph : Float -> Dist -> Form
graph horiz d = toList d |> map (\(v,p) -> (v,p*horiz)) |> graphBar

transform : Transform2D -> Form -> Form
transform t x = groupTransform t [x]

-- Diagrams
diagrams : [Form]
diagrams = [ bias |> graph 6
           , fate |> graph 6
           , ndk 2 d6 |> graph 6 ]

-- (stack h sp) vertically stacks forms which are h high with sp spacing.
stack : Float -> Float -> [Form] -> [Form]
stack h sp = mapi (\i f -> moveY (toFloat i * (h+sp)) f)

-- Program
main = collage 410 310
       [ rect 410 310 |> outlined (solid black)
       , diagrams
         |> stack 1 0.1
         |> group |> move (-0.5, -0.5)
         |> scale 50
       ]

{-
main = collage 410 210
       [ ndk 5 d6 `plus` always -17 |> graph 1
         |> move (-0.5, 0)
         |> transform (scaleXY 380 100)
       , rect 410 210 |> outlined (solid black)
       ]
-}
--main = plainText <| show <| bounds <| ndk 2 d6
