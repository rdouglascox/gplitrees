> module DataProp (Prop (..), Pred(..)) where

> data Prop = Atom Pred String
>           | Neg Prop
>           | Conj Prop Prop
>           | Disj Prop Prop
>           | Cond Prop Prop
>           | Bicon Prop Prop
>           | Exi Char Prop
>           | Uni Char Prop
>           deriving (Show, Eq)

> data Pred = Pred1 Char
>           | Pred2 Char
>           | Pred3 Char
>           deriving (Show, Eq)




