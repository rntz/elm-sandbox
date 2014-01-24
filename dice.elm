import Dict (Dict)
import Dict as D
import Graphics.Input as Input
import Maybe
import Regex (Regex)
import Regex as Re
import String
import String (toInt)
import Transform2D (Transform2D, matrix, identity)

import open Util

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

modify x = plus (always x)

mapDist : (Int -> Int) -> Dist -> Dist
mapDist f d = toList d |> map (\(x,p) -> (f x, p)) |> fromList

negate = mapDist (\x -> -x)

sumDists : [Dist] -> Dist
sumDists = foldr plus (always 0)

d6 = uniform [1..6]
dFate = uniform [-1..1]

ndk : Int -> Dist -> Dist
ndk n d = case n of
            0 -> always 0
            1 -> d
            _ -> d `plus` ndk (n-1) d

fate = ndk 4 dFate
fatevs = plus fate fate         -- fate == negate fate
bias = always -7 `plus` ndk 2 d6

-- Applying overlays to distributions
type Overlay = [Int]

classify : Overlay -> Int -> Int
classify o i = fromMaybe (length o) <| index (\x -> i <= x) o

overlay : Overlay -> Dist -> Dist
overlay o = mapDist (classify o)

-- Overlays for low & hi stakes
loOverlay = [6, 7, 10]
hiOverlay = [6, 9, 11]
awOverlay = [6, 9]


-- Parsing dice, quick-and-dirty.
dieRe : Regex
-- groups: [sign, number, kind]
dieRe = Re.pattern <|
        "([-+])? *([0-9]+)(?:d(" ++ kindRes ++ "))?"
-- a kind can be a number of sides, F for fate, or a list of values
kindRes = "[0-9]+|F|\\[ *(?:[-+]? *[0-9]+[ ,]*)+\\]"
intRe = Re.pattern "[-+]? *[0-9]+"

parseDice : String -> Dist
parseDice s = sumDists <| map parseDie <| Re.findAll dieRe s

parseDie : Re.Match -> Dist
parseDie m = let [signM, Just numS, kindM] = m.submatches
                 sign = case signM of
                          Just "-" -> negate
                          _ -> id
                 intify = fromMaybe 0 . toInt
                 num = intify numS
                 --pKind x = uniform [1 .. intify x `max` 1]
             in sign <|
                (case kindM of
                   Nothing -> always num
                   Just r -> ndk num (parseKind r))

parseKind : String -> Dist
parseKind s =
    if | "F" == s -> dFate
       | "[" == String.left 1 s ->
           (uniform . justs . map (toInt . .match) <|
            Re.findAll intRe s)
       | otherwise -> case toInt s of
                       Just n -> uniform [1 .. n `max` 1]
                       Nothing -> always 0

parseOverlay : String -> Maybe Overlay
parseOverlay s = let ms = Re.findAll intRe s
                     ns = justs <| map (toInt . .match) ms
                 in if length ms == length ns && length ns > 0
                    then Just ns
                    else Nothing


-- Visualization tools
-- weird 2^i = 1/(1+i)
--weird : Float -> Float
--weird x = 1/(1 + lg x)

-- weird _ _ 0 = 1
-- weird b r r = b
-- weird b r (2r) = b^2
-- weird b r (3r) = b^3
weird : Float -> Float -> Float -> Float
weird b r x = b^(x/r)

intColor : Int -> Color
intColor i =
  let j = toFloat (abs i)
      hue = turns (j / 7.0)
      --sat = 1 - weird 0.45 1 j
      sat = 1 - weird (1/3) 1 j
      val = clamp 1 1 <| weird (4/5) 6 j
  in hsv hue sat val

intColorGray : Int -> Color
intColorGray i =
    let j = toFloat (abs i)
        val = weird (2/3) 1 (j+0.2)
    in hsv 0.0 0.0 val

scaleXY : Float -> Float -> Transform2D
scaleXY x y = matrix x 0 0 y 0 0
scaleX x = scaleXY x 1.0
scaleY y = scaleXY 1.0 y

transform : Transform2D -> Form -> Form
transform t x = groupTransform t [x]

type GraphConfig = { colors: Int -> Color
                   , numbers: Bool }

defaultCfg = { colors = intColor, numbers = True }

graph : Float -> Float -> GraphConfig -> Dist -> Form
graph w h cfg d =
  let style = Text.height (h*0.36)
      part (v,p) rest =
        let txt = text . style . toText <| show v
            -- If not enough space for text plus 2px, omit it.
            txtF = if not cfg.numbers || 2 + widthOf txt > floor (p*w)
                   then group []
                   else txt |> toForm |> move (p*w/2, h/2)
        in group [ rect (p*w) h |> filled (cfg.colors v) |> move ((p*w)/2, h/2)
                 , txtF
                 , rest |> move (p*w, 0) ]
  in toList d |> foldr part (group []) |> move (-w/2, -h/2)


-- Diagramming distributions
diagram : String -> Dist -> Element
diagram name d =
  let bar = collage 600 50
            [ graph 600 50 defaultCfg d
            , rect 600 50 |> outlined (solid black) ]
      lblCfg = defaultLabelCfg
  in label lblCfg (toText name |> monospace . bold) bar

diagramFamily : [(String, Dist)] -> Element
diagramFamily ds =
    let maxLen = maximum <| map (String.length . fst) ds
        cfg = { defaultCfg | colors <- intColor, numbers <- False }
        lblCfg = { defaultLabelCfg | heightFactor <- 0.75
                                   , widthFactor <- 0.8 * toFloat maxLen }
        bar d = collage 300 20 [ graph 300 20 cfg d
                               , rect 300 20 |> outlined (solid black)
                               ]
        line (s,d) = label lblCfg (toText s |> bold) (bar d)
    in flow down <| map line ds

-- Controls
(diceField, diceInput) = Input.field "Dice"
(modField, modInput) = Input.field "Modifier"
(loField, loInput) = Input.field "Lo Overlay"
(hiField, hiInput) = Input.field "Hi Overlay"

userDist = parseDice . (\x -> if x == "" then "2d6" else x) <~ diceInput
userMod = modInput ~> fromMaybe 0 . toInt
userLo = loInput ~> fromMaybe loOverlay . parseOverlay
userHi = hiInput ~> fromMaybe hiOverlay . parseOverlay

userModDist = modify <~ userMod ~ userDist

-- Diagrams
diagrams : [Signal (String, Dist)]
diagrams = [ constant ("Fate", fate)
           , constant ("Fate vs", fatevs)
           -- , constant ("2d6-7", bias)
           , (,) "User" <~ userModDist
           , (,) "Hi" <~ (overlay <~ userHi ~ userModDist)
           , (,) "Lo" <~ (overlay <~ userLo ~ userModDist)
           , (,) "Fate vs" . overlay [-1,0,2] . plus fatevs . always <~ userMod
           , (,) "Fate" . overlay [-1,0,2] . plus fate . always <~ userMod
           -- , (,) "AW" . overlay awOverlay <~ userModDist
           ]

modFamily : [Int] -> Overlay -> Dist -> Element
modFamily ns o d =
    ns |> map (\n -> (show n, overlay o (modify n d)))
    |> diagramFamily

typeFamily : Int -> Signal Element
typeFamily n =
    [ (,) "Hi" <~ (overlay <~ userHi ~ (modify n <~ userDist))
    , (,) "Lo" <~ (overlay <~ userLo ~ (modify n <~ userDist))
    , constant ("Fate vs", overlay [-1,0,2] <| modify n fatevs)
    , constant ("Fate", overlay [-1,0,2] <| modify n fate)
    ] |> combine |> lift (labelS (show n) . diagramFamily)

mrng = [-2..3]

-- Program
main = (map (lift <| uncurry diagram) diagrams
        ++
        -- (let lbl = label {defaultLabelCfg | heightFactor <- 0.2} . toText in
        -- [ lbl "Hi" <~ (modFamily mrng <~ userHi ~ userDist)
        -- , lbl "Lo" <~ (modFamily mrng <~ userLo ~ userDist)
        -- , constant <| lbl "Fate vs" <| modFamily mrng [-1,0,2] fatevs
        -- , constant <| lbl "Fate" <| modFamily mrng [-1,0,2] fate
        -- ])
        -- ++
        [ beside (plainText "Dice: ") <~ diceField
        , beside (plainText "Mod: ") <~ modField
        , beside (plainText "Lo: ") <~ loField
        , beside (plainText "Hi: ") <~ hiField
        ] ++
        map typeFamily mrng ++
        [constant <| spacer 100 100])
       |> combine |> lift (stack 10)

--main = plainText <| show <| bounds <| ndk 2 d6
