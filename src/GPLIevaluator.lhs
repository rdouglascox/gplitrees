> module GPLIevaluator (val, genassignment, parser, Model (..)) where

> import GPLIparser
> import Data.List

We use Haskell's "Record Syntax" to define out data type for Models.

> data Model = Model {domain :: [Int]
>                    ,referents :: [(Char, Int)]
>                    ,extensions' :: [(Char, [Int])]
>                    ,extensions'' :: [(Char, [(Int,Int)])]
>                    ,extensions''' :: [(Char, [(Int,Int,Int)])]
>                    }
>                    deriving (Show)

Okay, the nice thing about the "Record Syntax" definition of our type for models is that qwe get functions from models to particular values for free. So `domain mymodel` will evaluate to `[1,2,3,4,5]`. 

Okay, the values we get back from model functions are lists of characters and pairs. They key value pairs. So it would be good to have a general function for getting the value of some key. Let's call it `myfind`:

> myfind k t = head [ v | (k', v) <- t, k == k'] 

Okay, cool, that will be handy. Now we need to write a function for interpreting terms.

Now, here's our function for terms.

> iterms :: Model -> Assignment -> [Char] -> [Int]
> iterms m g xs = map f xs
>     where f x = if x `elem` ['a'..'t']
>                 then myfind x (referents m)
>                 else myfind x g      

Okay, now we need a function for predicates.

> ipreds' :: Model -> Char -> [Int]
> ipreds' m x = myfind x (extensions' m)  

> ipreds'' :: Model -> Char -> [(Int,Int)]
> ipreds'' m x = myfind x (extensions'' m)  

> ipreds''' :: Model -> Char -> [(Int,Int,Int)]
> ipreds''' m x = myfind x (extensions''' m)  


And now we want to define the valuation function.

> val :: Model -> Assignment -> Prop -> Bool
> val m g (Atom (Pred1 x) y) = (tosing (iterms m g y)) `elem`  (ipreds' m x) 
> val m g (Atom (Pred2 'I') y) = (topair (iterms m g y)) `elem` (identity m)
> val m g (Atom (Pred2 x) y) = (topair (iterms m g y)) `elem` (ipreds'' m x) 
> val m g (Atom (Pred3 x) y) = (totriple (iterms m g y)) `elem` (ipreds''' m x)
> val m g (Neg scope) = not (val m g scope)
> val m g (Conj left right) = and [val m g left, val m g right] 
> val m g (Disj left right) =  or [val m g left, val m g right]
> val m g (Cond first second) = or [not (val m g first), val m g second]
> val m g (Bicon left right) = and [val m g (Cond left right),val m g (Cond right left)] 
> val m g (Exi x scope) = or (map (\z -> (val m (newa g x z) scope )) (domain m))
> val m g (Uni x scope) = and (map (\z -> (val m (newa g x z) scope )) (domain m))

We need a helper function which takes a list and puts each bit in a tuple.

> tosing (x:[]) = x
> topair (x:y:[]) = (x,y)
> totriple (x:y:z:[]) = (x,y,z) 

And we need a function which gets the extension of I^2 on a model. 

> identity :: Model -> [(Int,Int)]
> identity x = [(x,x) | x <- (domain x)]  

We need a reassignment function. The following type should be read something like this: takes an assignment and replaces the assignment of 'x' so that it maps to 1 in the new assignment. 

> newa :: Assignment -> Char -> Int -> Assignment
> newa g c i = [(c,i)] ++ [ x | x <- g, (fst x) /= c]   

Cool, so what we'll want is to specify the evluation function for each of the above. Something like this. (We will need to relativise to a Model and Assignment eventually).

That's it!

Well, actually, we want to auto-generate our assignment for variables. At this stage, we will just ask for a list of characters in use as variables, a model, and return an assignment.

> genassignment :: Model -> [Char] -> Assignment
> genassignment m c = [(x, head (domain m)) | x <- (getvars c)]
>     where  getvars xs = [ x | x <- xs, x `elem` ['u','w'..'z']]

> type Assignment = [(Char,Int)] 

