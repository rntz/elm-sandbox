module DynList where

type DynList a = { nextUid: Int, list: [(Int, a)] }

empty : DynList a
empty = { nextUid = 0, list = [] }

get : DynList a -> [a]
get dl = map snd dl.list

pushNew : (Int -> a) -> DynList a -> DynList a
pushNew f dl = { dl | nextUid <- dl.nextUid + 1
                    , list <- (dl.nextUid, f dl.nextUid) :: dl.list }

push : a -> DynList a -> (Int, DynList a)
push x dl = (dl.nextUid,
             { dl | nextUid <- dl.nextUid + 1
                  , list <- (dl.nextUid, x) :: dl.list })

remove : Int -> DynList a -> DynList a
remove i dl = { dl | list <- filter (\(j,_) -> i /= j) dl.list }
