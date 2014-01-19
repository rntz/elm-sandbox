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

-- Normalizes & deduplicates.
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

-- Outcomes in new system
results = [-1..2]
resultFailure = -1
resultPartial = 0
resultSuccess = 1
resultWithstyle = 2


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
      sat = 1 - weird (1/3) 1 j
      val = clamp 1 1 <| weird (4/5) 6 j
  in hsv hue sat val

scaleXY : Float -> Float -> Transform2D
scaleXY x y = matrix x 0 0 y 0 0
scaleX x = scaleXY x 1.0
scaleY y = scaleXY 1.0 y

graph : Float -> Float -> Dist -> Form
graph w h d =
  let style = Text.height (h*0.36)
      part (v,p) rest =
        let txt = text . style . toText <| show v
            -- If not enough space for text plus 2px, omit it.
            txtF = if 2 + widthOf txt > floor (p*w)
                   then group []
                   else txt |> toForm |> move (p*w/2, h/2)
        in group [ rect (p*w) h |> filled (intColor v) |> move ((p*w)/2, h/2)
                 , txtF
                 , rest |> move (p*w, 0) ]
  in toList d |> foldr part (group []) |> move (-w/2, -h/2)

transform : Transform2D -> Form -> Form
transform t x = groupTransform t [x]

-- Diagrams
diagrams : [(String, Dist)]
diagrams = [("2d6-7", bias),
            ("Fate", fate),
            ("2d6", ndk 2 d6)]

diagram : String -> Dist -> Element
diagram name d =
  let bar = collage 400 50
            [ graph 400 50 d
            , rect 400 50 |> outlined (solid black) ]
      txt = centered (monospace . bold <| toText name) |>
            container 60 50 middle
  in flow right [ txt, spacer 15 1, bar ]

stack : Int -> [Element] -> Element
stack sp elts = flow down <| intersperse (spacer 1 sp) elts

-- Program
main = stack 10 <| map (uncurry diagram) diagrams

--main = plainText <| show <| bounds <| ndk 2 d6
