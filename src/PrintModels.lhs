> module PrintModels (printmodel) where

Okay, so this module improves our approach to prettyprinting models. It depends on the data declaration for models which is exported by GPLI evaluator. We we begin by importing that module:

> import GPLIevaluator
> import Data.List

> printmodel :: Model -> String
> printmodel m = "model:" ++ "\n  domain: " ++ printdom m ++ printrefs m ++ "\n  extensions:" ++ printextensions m  

> printdom :: Model -> String
> printdom m = show (domain m)

> printrefs :: Model -> String
> printrefs m = if not (null (referents m))
>               then ("\n  referents:\n    " ++ (printpairs' (referents m)))
>               else ""

> pext1 :: Model -> String
> pext1 m = if not (null (extensions' m))
>           then ("\n    one-place:\n      " ++ (printpairs (extensions' m)))
>           else ""

> pext2 :: Model -> String
> pext2 m = if not (null (extensions'' m))
>           then ("\n    two-place:\n      " ++ (printpairs (extensions'' m)))
>           else ""

> pext3 :: Model -> String
> pext3 m = if not (null (extensions''' m))
>           then ("\n    three-place:\n      " ++ (printpairs (extensions''' m)))
>           else ""

> printpairs xs = concat $ intersperse "\n      " (map pairs xs)
>     where pairs x = [fst x] ++ ": " ++ show (snd x)

> printpairs' xs = concat $ intersperse "\n    " (map pairs xs)
>     where pairs x = [fst x] ++ ": " ++ show (snd x)

> printextensions m = pext1 m ++ pext2 m ++ pext3 m

> test = Model [1,2,3] [] [('F',[1,2]),('G',[3,2])] [('H',[(1,2)])] []
