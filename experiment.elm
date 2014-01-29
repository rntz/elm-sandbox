import Graphics.Input as Input

(ae,a) = Input.button "Button A"
(be,b) = Input.button "Button B"

both : Signal ((),())
both = (,) <~ a ~ b

ax = fst <~ both
bx = snd <~ both

main = flow down <~
       combine
       [ constant ae, constant be
       , plainText . (++) "A: " . show <~ count a
       , plainText . (++) "B: " . show <~ count b
       , plainText . (++) "AX: " . show <~ count ax
       , plainText . (++) "BX: " . show <~ count bx
       ]
