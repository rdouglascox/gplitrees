> module GPLIprinter (printtree) where

> import qualified Data.Tree
> import DataProp
> import DataTree
> import Data.List

> printprop :: Prop -> String
> printprop (Atom (Pred1 c) p) = c : p 
> printprop (Atom (Pred2 c) p) = c : p 
> printprop (Atom (Pred3 c) p) = c : p 
> printprop (Neg p) = "~" ++ printprop p
> printprop (Conj p q) = "(" ++ printprop p ++ "&" ++ printprop q ++ ")"
> printprop (Disj p q) = "(" ++ printprop p ++ "v" ++ printprop q ++ ")"
> printprop (Cond p q) = "(" ++ printprop p ++ "->" ++ printprop q ++ ")"
> printprop (Bicon p q) = "(" ++ printprop p ++ "<->" ++ printprop q ++ ")"
> printprop (Exi c p) = "#" ++ [c] ++ printprop p 
> printprop (Uni c p) = "@" ++ [c] ++ printprop p

> printelem :: Elem -> String
> printelem (Elem prop sub True) = (printprop prop) ++ (ifsub sub) ++ " \x2713" 
> printelem (Elem prop sub False) = (printprop prop) ++ "" ++ (ifsub sub)

> ifsub x = if not (null x)
>           then (" \\" ++ x)  
>           else []

> printelems :: [Elem] -> String
> printelems xs = concat (intersperse ", " (map printelem xs))

> printabletree :: Tree -> Data.Tree.Tree String
> printabletree (Branch el []) = Data.Tree.Node ((printelems el) ++ " <- o") []
> printabletree (Branch el [Branch [] []]) = Data.Tree.Node ((printelems el) ++ " \x2717") []
> printabletree (Branch el ts) = Data.Tree.Node (printelems el) (map printabletree ts)    

> printtree :: Tree -> String
> printtree t = Data.Tree.drawTree (printabletree t)
