> module DataTree (Tree (..), Elem (..)) where

> import DataProp

> data Tree = Branch [Elem] [Tree]
>           deriving (Show, Eq)

Here is our type for elements appearing on the tree:

> data Elem = Elem {prop :: Prop
>                  ,subs :: String
>                  ,check :: Bool
>                  } deriving (Show, Eq)


