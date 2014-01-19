type Dist a = [(a, Float)]

-- for now, just normalizes. here in case we change def'n of Dist later.
-- no duplicates permitted
fromList : [(a, Float)] -> Dist a
fromList l = let total = sum <| map snd l
             in l |> map (\ (a,p) -> (a, p / total))

toList : Dist a -> [(a, Float)]
toList x = x

always : a -> Dist a
always x = [(x,1)]

uniform : [a] -> Dist a
uniform xs = fromList <| map (\x -> (x,1)) xs

chance : a -> Dist a -> Float
chance x l = l |> filter (\(y,_) -> x == y)
             |> map snd |> sum

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

-- Some useful probability distributions
joint : Dist a -> Dist b -> Dist (a,b)
joint x y = (,) <$> x <*> y

replicateM : Int -> Dist a -> Dist [a]
replicateM n x = case n of
                   0 -> pure []
                   _ -> (::) <$> x <*> replicateM (n-1) x

ndk : Int -> Dist number -> Dist number
ndk n k = sum <$> replicateM n k

d6 = uniform [1..6]

main = plainText <| show d6
