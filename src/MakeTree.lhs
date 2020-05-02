> module MakeTree (Tree(..),Elem(..),SmartTree(..),maketree,imaketree,iimaketree,treestats,readmodels) where

> import DataProp
> import DataTree
> import Data.List
> import GPLIparser
> import GPLIprinter
> import GPLIevaluator
> import PrintModels
> import Control.Concurrent
> import Control.Parallel.Strategies
> import System.Console.ANSI

FOR PARALLELISM

> myparMap :: (a->b) -> [a] -> Eval [b]
> myparMap f [] = return []
> myparMap f (a:as) = do
>     b <- rpar (f a)
>     bs <- myparMap f as
>     return (b:bs)

> mymap x y = runEval (myparMap x y)

> myconcatMap x y = concat (mymap x y)

GENERAL FUNCTIONS

Here is a function which adds a list of elements to all open paths on a tree.


> addnonbranching :: [Elem] -> Tree -> Tree
> addnonbranching center (Branch [] [] ) = Branch [] []
> addnonbranching center (Branch ys []) = Branch (ys++center) []
> addnonbranching center (Branch ys ts) = Branch ys (map (addnonbranching center) ts)

Here is a function which adds a left list and a right list to new branches on all open paths on a tree.

> addbranching :: ([Elem],[Elem]) -> Tree -> Tree
> addbranching (left, right) (Branch [] []) = Branch [] []
> addbranching (left, right) (Branch ys []) = Branch ys [Branch right [], Branch left []]
> addbranching (left, right) (Branch ys ts) = Branch ys ((map (addbranching (right, left)) ts))

CONJUNCTION

> hasconj :: [Elem] -> Bool
> hasconj xs = any isconj xs
>    where isconj (Elem (Conj _ _) _ False) = True
>          isconj (Elem _ _ _ ) = False

> checkconj :: [Elem] -> [Elem]
> checkconj ((Elem (Conj left right) subs False ):xs) =  ((Elem (Conj left right) subs True ):xs) 
> checkconj (x:xs) = x : checkconj xs
> checkconj [] = []

> getconjuncts :: [Elem] -> [Elem]
> getconjuncts ((Elem (Conj left right) subs False ):xs) =  [Elem left [] False, Elem right [] False]  
> getconjuncts (x:xs) = getconjuncts xs
> getconjuncts [] = []

> applyconj :: Tree -> Tree
> applyconj (Branch elems xs) = if hasconj elems
>     then addnonbranching (getconjuncts elems) (Branch (checkconj elems) xs)
>     else (Branch elems (map applyconj xs))

DOUBLE NEG

> hasdneg :: [Elem] -> Bool
> hasdneg xs = any isdneg xs
>    where isdneg (Elem (Neg (Neg  _ )) _ False) = True
>          isdneg (Elem _ _ _ ) = False

> checkdneg :: [Elem] -> [Elem]
> checkdneg ((Elem (Neg (Neg scope )) subs False ):xs) =  ((Elem (Neg (Neg scope ))  subs True ):xs) 
> checkdneg (x:xs) = x : checkdneg xs
> checkdneg [] = []

> getdneg :: [Elem] -> [Elem]
> getdneg ((Elem (Neg (Neg scope )) subs False ):xs) =  [Elem scope [] False]  
> getdneg (x:xs) = getdneg xs
> getdneg [] = []

> applydneg :: Tree -> Tree
> applydneg (Branch elems xs) = if hasdneg elems
>     then addnonbranching (getdneg elems) (Branch (checkdneg elems) xs)
>     else (Branch elems (map applydneg xs))

NEGATED CONJUNCTION

> hasnegconj :: [Elem] -> Bool
> hasnegconj xs = any isnegconj xs
>    where isnegconj (Elem (Neg (Conj _ _)) _ False) = True
>          isnegconj (Elem _ _ _ ) = False

> checknegconj :: [Elem] -> [Elem]
> checknegconj ((Elem (Neg (Conj left right)) subs False ):xs) =  ((Elem (Neg (Conj left right)) subs True ):xs) 
> checknegconj (x:xs) = x : checknegconj xs
> checknegconj [] = []

> getnegconjuncts :: [Elem] -> ([Elem],[Elem])
> getnegconjuncts ((Elem (Neg (Conj left right)) subs False ):xs) =  ([Elem (Neg left) [] False], [Elem (Neg right) [] False])  
> getnegconjuncts (x:xs) = getnegconjuncts xs
> getnegconjuncts [] = ([],[])

> applynegconj :: Tree -> Tree
> applynegconj (Branch elems xs) = if hasnegconj elems
>     then addbranching (getnegconjuncts elems) (Branch (checknegconj elems) xs)
>     else (Branch elems (map applynegconj xs))

DISJUNCTION

> hasdisj :: [Elem] -> Bool
> hasdisj xs = any isdisj xs
>    where isdisj (Elem (Disj _ _) _ False) = True
>          isdisj (Elem _ _ _ ) = False

> checkdisj :: [Elem] -> [Elem]
> checkdisj ((Elem (Disj left right) subs False ):xs) =  ((Elem (Disj left right) subs True ):xs) 
> checkdisj (x:xs) = x : checkdisj xs
> checkdisj [] = []

> getdisjuncts :: [Elem] -> ([Elem],[Elem])
> getdisjuncts ((Elem (Disj left right) subs False ):xs) =  ([Elem left [] False], [Elem right [] False])
> getdisjuncts (x:xs) = getdisjuncts xs
> getdisjuncts [] = ([],[])

> applydisj :: Tree -> Tree
> applydisj (Branch elems xs) = if hasdisj elems
>     then addbranching (getdisjuncts elems) (Branch (checkdisj elems) xs)
>     else (Branch elems (map applydisj xs))

NEG DISJUNCTION

> hasnegdisj :: [Elem] -> Bool
> hasnegdisj xs = any isnegdisj xs
>    where isnegdisj (Elem (Neg (Disj _ _)) _ False) = True
>          isnegdisj (Elem _ _ _ ) = False

> checknegdisj :: [Elem] -> [Elem]
> checknegdisj ((Elem (Neg (Disj left right)) subs False ):xs) =  ((Elem (Neg (Disj left right)) subs True ):xs) 
> checknegdisj (x:xs) = x : checknegdisj xs
> checknegdisj [] = []

> getnegdisjuncts :: [Elem] -> [Elem]
> getnegdisjuncts ((Elem (Neg (Disj left right)) subs False ):xs) =  [Elem (Neg left) [] False, Elem (Neg right) [] False]  
> getnegdisjuncts (x:xs) = getnegdisjuncts xs
> getnegdisjuncts [] = []

> applynegdisj :: Tree -> Tree
> applynegdisj (Branch elems xs) = if hasnegdisj elems
>     then addnonbranching (getnegdisjuncts elems) (Branch (checknegdisj elems) xs)
>     else (Branch elems (map applynegdisj xs))

CONDITIONAL

> hascond :: [Elem] -> Bool
> hascond xs = any iscond xs
>    where iscond (Elem (Cond _ _) _ False) = True
>          iscond (Elem _ _ _ ) = False

> checkcond :: [Elem] -> [Elem]
> checkcond ((Elem (Cond left right) subs False ):xs) =  ((Elem (Cond left right) subs True ):xs) 
> checkcond (x:xs) = x : checkcond xs
> checkcond [] = []

> getconelements :: [Elem] -> ([Elem],[Elem])
> getconelements ((Elem (Cond left right) subs False ):xs) =  ([Elem (Neg left) [] False], [Elem right [] False]) 
> getconelements (x:xs) = getconelements xs
> getconelements [] = ([],[])

> applycond :: Tree -> Tree
> applycond (Branch elems xs) = if hascond elems
>     then addbranching (getconelements elems) (Branch (checkcond elems) xs)
>     else (Branch elems (map applycond xs))

NEGCONDITIONAL

> hasnegcond :: [Elem] -> Bool
> hasnegcond xs = any isnegcond xs
>    where isnegcond (Elem (Neg (Cond _ _)) _ False) = True
>          isnegcond (Elem _ _ _ ) = False

> checknegcond :: [Elem] -> [Elem]
> checknegcond ((Elem (Neg (Cond left right)) subs False ):xs) =  ((Elem (Neg (Cond left right)) subs True ):xs) 
> checknegcond (x:xs) = x : checknegcond xs
> checknegcond [] = []

> getnegconelements :: [Elem] -> [Elem]
> getnegconelements ((Elem (Neg (Cond left right)) subs False ):xs) =  [Elem left [] False, Elem (Neg right) [] False] 
> getnegconelements (x:xs) = getnegconelements xs
> getnegconelements [] = []

> applynegcond :: Tree -> Tree
> applynegcond (Branch elems xs) = if hasnegcond elems
>     then addnonbranching (getnegconelements elems) (Branch (checknegcond elems) xs)
>     else (Branch elems (map applynegcond xs))

BICONDITIONAL

> hasbicond :: [Elem] -> Bool
> hasbicond xs = any isbicond xs
>    where isbicond (Elem (Bicon _ _) _ False) = True
>          isbicond (Elem _ _ _ ) = False

> checkbicond :: [Elem] -> [Elem]
> checkbicond ((Elem (Bicon left right) subs False ):xs) =  ((Elem (Bicon left right) subs True ):xs) 
> checkbicond (x:xs) = x : checkbicond xs
> checkbicond [] = []

> getbiconelements :: [Elem] -> ([Elem],[Elem])
> getbiconelements ((Elem (Bicon left right) subs False ):xs) =  ([Elem left [] False, Elem right [] False],[Elem (Neg left) [] False, Elem (Neg right) [] False]) 
> getbiconelements (x:xs) = getbiconelements xs
> getbiconelements [] = ([],[])

> applybicond :: Tree -> Tree
> applybicond (Branch elems xs) = if hasbicond elems
>     then addbranching (getbiconelements elems) (Branch (checkbicond elems) xs)
>     else (Branch elems (map applybicond xs))

NEGBICONDITIONAL

> hasnegbicond :: [Elem] -> Bool
> hasnegbicond xs = any isnegbicond xs
>    where isnegbicond  (Elem (Neg (Bicon _ _)) _ False) = True
>          isnegbicond (Elem _ _ _ ) = False

> checknegbicond :: [Elem] -> [Elem]
> checknegbicond ((Elem (Neg (Bicon left right)) subs False ):xs) =  ((Elem (Neg (Bicon left right)) subs True ):xs) 
> checknegbicond (x:xs) = x : checknegbicond xs
> checknegbicond [] = []

> getnegbiconelements :: [Elem] -> ([Elem],[Elem])
> getnegbiconelements ((Elem (Neg (Bicon left right)) subs False ):xs) =  ([Elem left [] False, Elem (Neg right) [] False], [Elem (Neg left) [] False, Elem right [] False]) 
> getnegbiconelements (x:xs) = getnegbiconelements xs
> getnegbiconelements [] = ([],[])

> applynegbicond :: Tree -> Tree
> applynegbicond (Branch elems xs) = if hasnegbicond elems
>     then addbranching (getnegbiconelements elems) (Branch (checknegbicond elems) xs)
>     else (Branch elems (map applynegbicond xs))

DOUBLENEGATION

> hasdoublenegj :: [Elem] -> Bool
> hasdoublenegj xs = any isdoublenegj xs
>    where isdoublenegj (Elem (Neg (Neg _ )) _ False) = True
>          isdoublenegj (Elem _ _ _ ) = False

> checkdoublenegj :: [Elem] -> [Elem]
> checkdoublenegj ((Elem (Neg (Neg center)) subs False ):xs) =  ((Elem (Neg (Neg center)) subs True ):xs) 
> checkdoublenegj (x:xs) = x : checkdoublenegj xs
> checkdoublenegj [] = []

> getdoubleneg :: [Elem] -> [Elem]
> getdoubleneg ((Elem (Neg (Neg center)) subs False ):xs) =  [Elem center [] False]  
> getdoubleneg (x:xs) = getdoubleneg xs
> getdoubleneg [] = []

> applydoublenegj :: Tree -> Tree
> applydoublenegj (Branch elems xs) = if hasdoublenegj elems
>     then addnonbranching (getdoubleneg elems) (Branch (checkdoublenegj elems) xs)
>     else (Branch elems (map applydoublenegj xs))

NEGATED UNIVERSAL

> hasneguni :: [Elem] -> Bool
> hasneguni xs = any isneguni xs
>    where isneguni (Elem (Neg (Uni _ _)) _ False) = True
>          isneguni (Elem _ _ _ ) = False

> checkneguni :: [Elem] -> [Elem]
> checkneguni ((Elem (Neg (Uni var scope)) subs False ):xs) =  ((Elem (Neg (Uni var scope)) subs True ):xs) 
> checkneguni (x:xs) = x : checkneguni xs
> checkneguni [] = []

> getneguni :: [Elem] -> [Elem]
> getneguni ((Elem (Neg (Uni var scope)) subs False ):xs) =  [Elem (Exi var (Neg scope)) [] False]  
> getneguni (x:xs) = getneguni xs
> getneguni [] = []

> applyneguni :: Tree -> Tree
> applyneguni (Branch elems xs) = if hasneguni elems
>     then addnonbranching (getneguni elems) (Branch (checkneguni elems) xs)
>     else (Branch elems (map applyneguni xs))

NEGATED EXISTENTIAL

> hasnegexe :: [Elem] -> Bool
> hasnegexe xs = any isnegexe xs
>    where isnegexe (Elem (Neg (Exi _ _)) _ False) = True
>          isnegexe (Elem _ _ _ ) = False

> checknegexe :: [Elem] -> [Elem]
> checknegexe ((Elem (Neg (Exi var scope)) subs False ):xs) =  ((Elem (Neg (Exi var scope)) subs True ):xs) 
> checknegexe (x:xs) = x : checknegexe xs
> checknegexe [] = []

> getnegexe :: [Elem] -> [Elem]
> getnegexe ((Elem (Neg (Exi var scope)) subs False ):xs) =  [Elem (Uni var (Neg scope)) [] False]  
> getnegexe (x:xs) = getnegexe xs
> getnegexe [] = []

> applynegexe :: Tree -> Tree
> applynegexe (Branch elems xs) = if hasnegexe elems
>     then addnonbranching (getnegexe elems) (Branch (checknegexe elems) xs)
>     else (Branch elems (map applynegexe xs))

EXISTENTIAL and UNIVERSAL

First, two little helper functions.

> getnameselems :: String -> [Elem] -> String
> getnameselems ys xs = sort $ nub $ (ys ++ (concatMap getnameselem  xs))
>    where getnameselem (Elem p _ _) = getnames p 

> getprops :: [Prop] -> [Elem] -> [Prop]
> getprops ys xs = nub $ ys ++ (map getp xs)
>     where getp (Elem p _ _) = p

Now a new data-type. A smart-tree is a tree whose terminals on open paths carry information about all the names on the path on which they occur and all the propositions which occur on that path. (The former is for uni and exe, the latter is for id) 

This turns an ordinary tree into a smart tree:

 dumbtosmart :: String -> [Prop] -> Tree  -> SmartTree
 dumbtosmart xs ys (Branch es ts) = (SmartBranch es (map (dumbtosmart xs ys) ts) (getnameselems xs es) (getprops ys es))  

> dumbtosmart :: String -> [Prop] -> Tree -> SmartTree
> dumbtosmart xs ys (Branch [] []) = SmartBranch [] [] xs ys 
> dumbtosmart xs ys (Branch es []) = SmartBranch es [] ((getnameselems xs es))   (getprops ys es)
> dumbtosmart xs ys (Branch es ts) = SmartBranch es (map (dumbtosmart (nub(xs ++ (getnameselems xs es))) (nub(ys ++ (getprops ys es )))) ts) (getnameselems xs es) (getprops ys es)

This converts it back:

> smarttodumb :: SmartTree -> Tree
> smarttodumb (SmartBranch es sts xs ys) = (Branch es (map smarttodumb sts))

This gets all names on open paths.

> namesonopenpaths :: SmartTree -> String  
> namesonopenpaths (SmartBranch em [] n _) = n
> namesonopenpaths (SmartBranch em ts _ _) = concatMap namesonopenpaths ts   

This gets all propositions on open paths.

> propsonopenpaths :: SmartTree -> [Prop]
> propsonopenpaths (SmartBranch em [] _ p ) = p
> propsonopenpaths (SmartBranch em ts _ _ ) = concatMap propsonopenpaths ts

We need a general substitution function. We'll write it in GPLIparser (proposition to change -> target of the substitution -> substitution -> output prop)

EXISTENTIAL

For existential we substitute the next new name.

> nnames = ['a'..'t'] ++ ['\128' ..]

> nextnewname :: String -> Char
> nextnewname x = head $ filter (\y -> notElem y x) nnames   

> newonpaths :: SmartTree -> Char
> newonpaths = nextnewname . namesonopenpaths

The standard functions:

Checking to see whether a list of elements has an existential on it is straightforward:

> hasexi :: [Elem] -> Bool
> hasexi xs = any isexi xs
>    where isexi (Elem (Exi _ _) _ False) = True
>          isexi (Elem _ _ _ ) = False

Checking off the existential now involves feeding the function a Char to be entered into subs.

> checkexi :: Char -> [Elem] -> [Elem]
> checkexi y ((Elem (Exi var scope) subs False ):xs) =  ((Elem (Exi var scope) (y: subs) True ):xs) 
> checkexi y (x:xs) = x : checkexi y xs
> checkexi y [] = []

We have the function for geting the element to append to the tree take the relevant substitution as an argument too.

> getexi :: Char -> [Elem] -> [Elem]
> getexi y ((Elem (Exi var scope) subs False ):xs) =  [Elem (substitute scope var y) [] False]  
> getexi y (x:xs) = getexi y xs
> getexi y [] = []

Modify the nonbranching rule for SmartTrees

> addnonbranchingsmart :: [Elem] -> SmartTree -> SmartTree
> addnonbranchingsmart center (SmartBranch [] [] u v) = SmartBranch [] [] u v
> addnonbranchingsmart center (SmartBranch ys [] u v) = SmartBranch (ys++center) [] u v
> addnonbranchingsmart center (SmartBranch ys ts u v) = SmartBranch ys (map (addnonbranchingsmart center) ts) u v

Apply the existential rule to a SmartTree

> applyexismart :: SmartTree -> SmartTree
> applyexismart (SmartBranch elems xs u v) = if hasexi elems
>     then addnonbranchingsmart (getexi (newonpaths(SmartBranch elems xs u v)) elems) (SmartBranch (checkexi (newonpaths(SmartBranch elems xs u v)) elems) xs u v)
>     else (SmartBranch elems (map applyexismart xs) u v)

> applyexi :: Tree -> Tree
> applyexi x = smarttodumb $ applyexismart $ dumbtosmart [] [] x

UNIVERSAL

Okay, we need a function which tests whether the path a universal is on it saturated or not, relative to that universal.

> checknotsatuni :: SmartTree -> Elem -> Bool
> checknotsatuni t (Elem (Uni x y) subs False) = null subs || allfstinsnd subs (namesonopenpaths t)
> checknotsatuni t (Elem _ _ _) = False

> allfstinsnd :: String -> String -> Bool
> allfstinsnd x y = not (all (\x -> x == True) (map (\z -> z `elem` x) y))


Okay, so now "hasuni" checks for unsaturated universals.

> hasuni :: SmartTree -> [Elem] -> Bool
> hasuni t xs = any (checknotsatuni t) xs

Checking off the unistential now involves feeding the function a Char to be entered into subs.

> checkuni :: SmartTree -> [Elem] -> [Elem]
> checkuni y ((Elem (Uni var scope) subs False ):xs) = if checknotsatuni y (Elem (Uni var scope) subs False)
>    then ((Elem (Uni var scope) (subs ++ [(nextname subs y)]) False ):xs)
>    else [(Elem (Uni var scope) subs False)] ++  checkuni y xs 
> checkuni y (x:xs) = x : checkuni y xs
> checkuni y [] = []

We have the function for geting the element to append to the tree take the relevant substitution as an argument too.

> getuni :: SmartTree -> [Elem] -> [Elem]
> getuni y ((Elem (Uni var scope) subs False ):xs) = if checknotsatuni y (Elem (Uni var scope) subs False)
>     then  [Elem (substitute scope var (nextname subs y)) [] False]  
>     else getuni y xs
> getuni y (x:xs) = getuni y xs
> getuni y [] = []

> nextname :: String -> SmartTree -> Char
> nextname x t = head $ [ y | y <- (nub ((namesonopenpaths t) ++ nnames)), (y `notElem` x)]
 
Apply the universal rule to a SmartTree

> applyunismart :: SmartTree -> SmartTree
> applyunismart (SmartBranch elems xs u v) = if hasuni (SmartBranch elems xs u v) elems
>     then addnonbranchingsmart (getuni (SmartBranch elems xs u v) elems) (SmartBranch (checkuni (SmartBranch elems xs u v) elems) xs u v)
>     else (SmartBranch elems (map applyunismart xs) u v)

> applyuni :: Tree -> Tree
> applyuni x = smarttodumb $ applyunismart $ dumbtosmart [] [] x

SUBSTITUTION OF IDENTICALS!!!!!!

At some point we are going to need a function which takes the names in Iab (for identity) and a list of atomic propositions and negated atomics and generates the next new proposition in the list by substituting an a for a b or b for an a. Let's model it on nextname and nextnewname above

Okay, so the following will get us all the atomic propositions on the open paths of a tree.

> atomicsonpath :: SmartTree -> [Prop]
> atomicsonpath xs = concatMap isatomic (propsonopenpaths xs)
>     where isatomic (Atom x y) = [Atom x y]
>           isatomic (Neg (Atom x y)) = [Neg (Atom x y)]
>           isatomic _ = [] 

> genonesubs :: String -> [Prop] -> [Prop]
> genonesubs xs ps = map (genonesub xs) ps

> genonesub :: String -> Prop -> Prop 
> genonesub (fst:snd:[]) (Atom (Pred1 x) y) 
>    | y == [fst] = (Atom (Pred1 x) [snd])
>    | y == [snd] = (Atom (Pred1 x) [fst])
>    | otherwise =  (Atom (Pred1 x) y)   
> genonesub (fst:snd:[]) (Neg (Atom (Pred1 x) y))
>    | y == [fst] = Neg (Atom (Pred1 x) [snd])
>    | y == [snd] = Neg (Atom (Pred1 x) [fst])
>    | otherwise =  Neg (Atom (Pred1 x) y)   
> genonesub zx (Atom x y) = (Atom x y)
> genonesub xz (Neg (Atom x y)) = Neg (Atom x y)

> gentwosubs :: String -> [Prop] -> [Prop]
> gentwosubs xs ps = concatMap (gentwosub' xs) ps

> gentwosub' :: String -> Prop -> [Prop]
> gentwosub' y (Atom (Pred2 x) z) = gentwosub y (Atom (Pred2 x) z)
> gentwosub' y (Neg (Atom (Pred2 x) z)) = makenegs $ gentwosub y (Atom (Pred2 x) z)
> gentwosub' z (Atom x y) = [(Atom x y)]
> gentwosub' z (Neg (Atom x y)) = [Neg (Atom x y)]




> gentwosub :: String -> Prop -> [Prop] 
> gentwosub (fst:snd:[]) (Atom (Pred2 x) (fs:sn:[])) 
>     | fst == fs && fst == sn = [(Atom (Pred2 x) (fst:snd:[])), (Atom (Pred2 x) (snd:fst:[])),(Atom (Pred2 x) (snd:snd:[]))]   
>     | fst == fs && fst /= sn = [(Atom (Pred2 x) (snd:sn:[]))]
>     | fst /= fs && fst == sn = [(Atom (Pred2 x) (fs:snd:[]))]
>     | snd == fs && snd == sn = [(Atom (Pred2 x) (snd:fst:[])), (Atom (Pred2 x) (fst:snd:[])),(Atom (Pred2 x) (fst:fst:[]))]
>     | snd == fs && snd /= sn = [(Atom (Pred2 x) (fst:sn:[]))]
>     | snd /= fs && snd == sn = [(Atom (Pred2 x) (fs:fst:[]))]
> gentwosub zx (Atom x y) = [(Atom x y)]

> genthreesubs :: String -> [Prop] -> [Prop]
> genthreesubs xs ps = concatMap (genthreesub' xs) ps

> genthreesub' :: String -> Prop -> [Prop]
> genthreesub' y (Atom (Pred3 x) z) = genthreesub y (Atom (Pred3 x) z)
> genthreesub' y (Neg (Atom (Pred3 x) z)) = makenegs $ genthreesub y (Atom (Pred3 x) z)
> genthreesub' z (Atom x y) = [(Atom x y)]
> genthreesub' z (Neg (Atom x y)) = [Neg (Atom x y)]

> makenegs :: [Prop] -> [Prop]
> makenegs xs = [ Neg x | x <- xs] 

> genthreesub :: String -> Prop -> [Prop]
> genthreesub (fst:snd:[]) (Atom (Pred3 x) (fs:sn:tr:[]))
>     | fst == fs && fst == sn && fst == tr = [(Atom (Pred3 x) (snd:snd:snd:[])),(Atom (Pred3 x) (snd:fst:fst:[])),(Atom (Pred3 x) (snd:snd:fst:[])),(Atom (Pred3 x) (fst:fst:snd:[])),(Atom (Pred3 x) (snd:fst:snd:[]))]  
>     | fst /= fs && fst == sn && fst == tr = [(Atom (Pred3 x) (snd:snd:snd:[])),(Atom (Pred3 x) (fs:snd:fst:[])),(Atom (Pred3 x) (fs:fst:snd:[]))]  
>     | fst /= fs && fst /= sn && fst == tr = [(Atom (Pred3 x) (fs:sn:snd:[]))]  
>     | fst == fs && fst /= sn && fst == tr = [(Atom (Pred3 x) (snd:sn:snd:[])),(Atom (Pred3 x) (snd:sn:fst:[])),(Atom (Pred3 x) (fst:sn:snd:[]))]  
>     | fst == fs && fst /= sn && fst /= tr = [(Atom (Pred3 x) (snd:sn:tr:[]))]  
>     | fst == fs && fst == sn && fst /= tr = [(Atom (Pred3 x) (snd:snd:tr:[])),(Atom (Pred3 x) (fst:snd:tr:[])),(Atom (Pred3 x) (snd:fst:tr:[]))]  
>     | fst /= fs && fst == sn && fst /= tr = [(Atom (Pred3 x) (fs:snd:tr:[]))]
>     | snd == fs && snd == sn && snd == tr = [(Atom (Pred3 x) (fst:fst:fst:[])),(Atom (Pred3 x) (fst:fst:snd:[])),(Atom (Pred3 x) (fst:snd:snd:[])),(Atom (Pred3 x) (snd:fst:snd:[])),(Atom (Pred3 x) (snd:fst:fst:[]))]
>     | snd /= fs && snd == sn && snd == tr = [(Atom (Pred3 x) (fs:fst:fst:[])),(Atom (Pred3 x) (fs:fst:snd:[])),(Atom (Pred3 x) (fs:snd:fst:[]))]
>     | snd /= fs && snd /= sn && snd == tr = [(Atom (Pred3 x) (fs:sn:fst:[]))]
>     | snd == fs && snd /= sn && snd == tr = [(Atom (Pred3 x) (fst:sn:fst:[])),(Atom (Pred3 x) (fst:sn:snd:[])),(Atom (Pred3 x) (snd:sn:fst:[]))]
>     | snd == fs && snd /= sn && snd /= tr = [(Atom (Pred3 x) (fst:sn:tr:[]))]
>     | snd == fs && snd == sn && snd /= tr = [(Atom (Pred3 x) (fst:fst:tr:[])),(Atom (Pred3 x) (fst:snd:tr:[])),(Atom (Pred3 x) (snd:fst:tr:[]))]
>     | snd /= fs && snd == sn && snd /= tr = [(Atom (Pred3 x) (fs:fst:tr:[]))]
> genthreesub zx (Atom x y) = [(Atom x y)]

Now we can put all of these together to give us:

> gensubs :: String -> SmartTree -> [Prop]
> gensubs x y = ( genthreesubs x (atomicsonpath y) ++  gentwosubs x (atomicsonpath y) ++  genonesubs x (atomicsonpath y)) 

Now we need a function which checks if a path is saturated relative to a formula. To put it another way, we need a function to the True if a Elem has an ID such that gensubs generates a proposition which is no generated by atomicsonpath.

> allfstinsnd' :: [Prop] -> [Prop] -> Bool
> allfstinsnd' x y = not (all (\x -> x == True) (map (\z -> z `elem` x) y))

> hasid :: SmartTree -> [Elem] -> Bool
> hasid t xs = any (checknotsatid t) xs

> checknotsatid :: SmartTree -> Elem -> Bool
> checknotsatid t (Elem (Atom (Pred2 'I') y) subs False) = allfstinsnd'  (nub (atomicsonpath t)) (nub (gensubs y t))
> checknotsatid t (Elem _ _ _) = False

Okay, here's the rule at work:

Checking off the idstential now involves feeding the function a Char to be entered into subs.

We have the function for geting the element to append to the tree take the relevant substitution as an argument too.

> getid :: SmartTree -> [Elem] -> [Elem]
> getid y ((Elem (Atom (Pred2 'I') t) subs False ):xs) = if checknotsatid y (Elem (Atom (Pred2 'I') t) subs False)
>     then [Elem (nextnewprop t y) [] False]  
>     else getid y xs
> getid y (x:xs) = getid y xs
> getid y [] = []

> nextnewprop :: String -> SmartTree -> Prop  
> nextnewprop y t = head ((nub (gensubs y t) ) \\ (nub (atomicsonpath t)))

Apply the idversal rule to a SmartTree

> applyidsmart :: SmartTree -> SmartTree
> applyidsmart (SmartBranch elems xs u v) = if hasid (SmartBranch elems xs u v) elems
>     then addnonbranchingsmart (getid (SmartBranch elems xs u v) elems) (SmartBranch elems xs u v)
>     else (SmartBranch elems (map applyidsmart xs) u v)

> applyid :: Tree -> Tree
> applyid x = smarttodumb $ applyidsmart $ dumbtosmart [] [] x



CHECK FOR CONTRADICTIONS

> type Path = [[Elem]]

> getpaths :: Tree -> [Path]
> getpaths (Branch [] []) = []
> getpaths (Branch es []) = [[es]]
> getpaths (Branch es ts) = map ([es]++) (concatMap getpaths ts)

> getpropsonpath :: Path -> [Prop]
> getpropsonpath es = map getprop (concat es)
>     where getprop (Elem prop subs bool) = prop 

> newclosure :: [Prop] -> [Bool]
> newclosure xs = map isnegid xs
>     where isnegid (Neg (Atom (Pred2 'I') (x:y:[]))) = if x == y then True else False
>           isnegid _ = False


> checkpaths :: [Path] -> [Path]
> checkpaths xs = [ x | x <- xs, (checkpath (getpropsonpath x))]


> checkpath :: [Prop] -> Bool
> checkpath ps = (or (map (\x -> (x `elem` ps) && ((Neg x) `elem` ps)) ps)) || or (newclosure ps)

> killpath :: Path -> Tree -> Tree
> killpath (z:zs) (Branch [] []) = (Branch [] [])
> killpath [] (Branch [] []) = (Branch [] [])
> killpath (z:zs) (Branch es []) = if es == z
>     then (Branch es [Branch [] []])
>     else Branch es []
> killpath [] (Branch es []) = (Branch es [])
> killpath (z:zs) (Branch es ts) = if es == z
>     then Branch es (map (killpath zs) ts)
>     else Branch es ts 
> killpath [] (Branch es ts) = Branch es ts

> killpaths :: [Path] -> Tree -> Tree
> killpaths (y:ys) x = killpaths ys (killpath y x)
> killpaths [] x = x

> oldcfc :: Tree -> Tree
> oldcfc x = killpaths (checkpaths (getpaths x)) x  

> cfc = smartcfc

SMARTCFC

Profiling suggests that this method of checking for contradictions is not very efficient. Let's re-implement cfc in terms of smart-trees. After all, a smart tree caries with it information about all propositions on the path. We can just check these and change the path to a closed one. It should be more efficient this way. 

> smartcfc :: Tree -> Tree
> smartcfc t = smarttodumb (closepaths (dumbtosmart [] [] t))

> closepaths :: SmartTree -> SmartTree
> closepaths (SmartBranch [] [] ns ps) = SmartBranch [] [] ns ps 
> closepaths (SmartBranch es [] ns ps) = if checkpath ps
>                                            then SmartBranch es [SmartBranch [] [] ns ps] ns ps
>                                            else SmartBranch es [] ns ps  
> closepaths (SmartBranch es ts ns ps) = SmartBranch es (map closepaths ts) ns ps


ALL RULES

Branching

> disj xs = cfc (applydisj xs)
> negconj xs = cfc (applynegconj xs)
> con xs = cfc (applycond xs)
> bicon xs = cfc (applybicond xs)
> negbicon xs = cfc (applynegbicond xs)

NonBranching

> conj xs = cfc (applyconj xs)
> negdisj xs = cfc (applynegdisj xs)
> negcon xs = cfc (applynegcond xs)
> negsome xs = cfc (applynegexe xs)
> negall xs = cfc (applyneguni xs)

Double Negation

> dneg xs = cfc (applydneg xs)

Quantifiers

> exi xs = cfc (applyexi xs)
> uni xs = cfc (applyuni xs)


> loopdneg x | dneg x == dneg (dneg x) = dneg x
>            | otherwise = loopdneg (dneg x)

> allnon xs = loopdneg $ negall $ loopdneg $ negsome $ loopdneg $ negcon $ loopdneg $ negdisj $ loopdneg $ conj $ loopdneg xs

> loopnon x | allnon x == allnon (allnon x) = allnon x
>           | otherwise = loopnon (allnon x)

> alls' xs = allnon $ exi $ allnon $ negbicon $ allnon $ bicon $ allnon $ con $ allnon $ negconj $ allnon $ disj  $ allnon xs

> idd x = cfc (applyid x)

> loopid x | idd x == idd (idd x) = idd x
>          | otherwise = loopid (idd x)


> allrules xs = uni $ alls' xs

MAKETREE

> maketree' :: Tree -> Tree
> maketree' x | allrules x == allrules (allrules x) = allrules x
>            | otherwise = maketree' (allrules x)

> maketree :: String -> Tree
> maketree x = loopid $ maketree' (gentree (lines x))

END OF ORDINARY MAKETREE

INCREMENTAL MAKETREE I

Okay, the idea for the incremental maketree function is to generalise the above so that each rule takes a list of trees as input and produces a list of trees as output. If the rule applies, it returns the list with the new tree appended to the end. If it doesn't apply it returns the old list unchanged. We might as well do this properly. We will make the list not just a list of trees, but a list of pairs of trees and strings. The string says what rule has been applied.  

> igentree :: [String] -> [(Tree,String)]
> igentree xs = [(gentree xs,"setting up the tree...")]

Branching

> idisj xs = icfc (iapplydisj xs)
> inegconj xs = icfc (iapplynegconj xs)
> icon xs = icfc (iapplycon xs)
> ibicon xs = icfc (iapplybicon xs)
> inegbicon xs = icfc (iapplynegbicon xs)

> iapplydisj :: [(Tree,String)] -> [(Tree,String)]
> iapplydisj xs = if (fst (last xs)) /= applydisj (fst (last xs)) 
>                 then xs ++ [(applydisj (fst (last xs)),"applying the rule for disjunction...")]
>                 else xs

> iapplynegconj :: [(Tree,String)] -> [(Tree,String)]
> iapplynegconj xs = if (fst (last xs)) /= applynegconj (fst (last xs)) 
>                 then xs ++ [(applynegconj (fst (last xs)),"applying the rule for negated conjunction...")]
>                 else xs

> iapplycon :: [(Tree,String)] -> [(Tree,String)]
> iapplycon xs = if (fst (last xs)) /= applycond (fst (last xs)) 
>                 then xs ++ [(applycond (fst (last xs)),"applying the rule for conditional...")]
>                 else xs

> iapplybicon :: [(Tree,String)] -> [(Tree,String)]
> iapplybicon xs = if (fst (last xs)) /= applybicond (fst (last xs)) 
>                 then xs ++ [(applybicond (fst (last xs)),"applying the rule of biconditional...")]
>                 else xs

> iapplynegbicon :: [(Tree,String)] -> [(Tree,String)]
> iapplynegbicon xs = if (fst (last xs)) /= applynegbicond (fst (last xs)) 
>                 then xs ++ [(applynegbicond (fst (last xs)),"applying the rule for negative biconditional...")]
>                 else xs

NonBranching

> iconj xs = icfc (iapplyconj xs)
> inegdisj xs = icfc (iapplynegdisj xs)
> inegcon xs = icfc (iapplynegcon xs)
> inegsome xs = icfc (iapplynegsome xs)
> inegall xs = icfc (iapplynegall xs)

> iapplyconj :: [(Tree,String)] -> [(Tree,String)]
> iapplyconj xs = if (fst (last xs)) /= applyconj (fst (last xs)) 
>                 then xs ++ [(applyconj (fst (last xs)),"applying the rule for conjunction...")]
>                 else xs

> iapplynegdisj :: [(Tree,String)] -> [(Tree,String)]
> iapplynegdisj xs = if (fst (last xs)) /= applynegdisj (fst (last xs)) 
>                 then xs ++ [(applynegdisj (fst (last xs)),"applying the rule for negated disjunction...")]
>                 else xs

> iapplynegcon :: [(Tree,String)] -> [(Tree,String)]
> iapplynegcon xs = if (fst (last xs)) /= applynegcond (fst (last xs)) 
>                 then xs ++ [(applynegcond (fst (last xs)),"applying the rule for negated conditional...")]
>                 else xs

> iapplynegsome :: [(Tree,String)] -> [(Tree,String)]
> iapplynegsome xs = if (fst (last xs)) /= applynegexe (fst (last xs)) 
>                 then xs ++ [(applynegexe (fst (last xs)),"applying the rule for negated existential...")]
>                 else xs

> iapplynegall :: [(Tree,String)] -> [(Tree,String)]
> iapplynegall xs = if (fst (last xs)) /= applyneguni (fst (last xs)) 
>                 then xs ++ [(applyneguni (fst (last xs)),"applying the rule for negated universal...")]
>                 else xs

Double Negation

> idneg xs = icfc (iapplydneg xs)

> iapplydneg :: [(Tree,String)] -> [(Tree,String)]
> iapplydneg xs = if (fst (last xs)) /= applydneg (fst (last xs)) 
>                 then xs ++ [(applydneg (fst (last xs)),"applying the rule for double negation...")]
>                 else xs

Quantifiers

> iexi xs = icfc (iapplyexi xs)
> iuni xs = icfc (iapplyuni xs)

> iapplyexi :: [(Tree,String)] -> [(Tree,String)]
> iapplyexi xs = if (fst (last xs)) /= applyexi (fst (last xs)) 
>                 then xs ++ [(applyexi (fst (last xs)),"applying the rule for existential...")]
>                 else xs

> iapplyuni :: [(Tree,String)] -> [(Tree,String)]
> iapplyuni xs = if (fst (last xs)) /= applyuni (fst (last xs)) 
>                 then xs ++ [(applyuni (fst (last xs)),"applying the rule for universal...")]
>                 else xs


> icfc :: [(Tree,String)] -> [(Tree,String)]
> icfc xs = if (fst (last xs)) /= cfc (fst (last xs)) 
>                 then xs ++ [(cfc (fst (last xs)),"applying closure rule...")]
>                 else xs


> iloopdneg x | idneg x == idneg (idneg x) = idneg x
>             | otherwise = iloopdneg (idneg x)

> iallnon xs = iloopdneg $ inegall $ iloopdneg $ inegsome $ iloopdneg $ inegcon $ iloopdneg $ inegdisj $ iloopdneg $ iconj $ iloopdneg xs

> iloopnon x | iallnon x == iallnon (iallnon x) = iallnon x
>           | otherwise = iloopnon (iallnon x)

> ialls' xs = iallnon $ iexi $ iallnon $ inegbicon $ iallnon $ ibicon $ iallnon $ icon $ iallnon $ inegconj $ iallnon $ idisj  $ iallnon xs

> iidd x = icfc (iapplyid x)

> iapplyid :: [(Tree,String)] -> [(Tree,String)]
> iapplyid xs = if (fst (last xs)) /= applyid (fst (last xs)) 
>                 then xs ++ [(applyid (fst (last xs)),"applying the substitution of identicals rule...")]
>                 else xs

> iloopid x | iidd x == iidd (iidd x) = iidd x
>          | otherwise = iloopid (iidd x)

> iallrules xs = iuni $ ialls' xs

MAKETREE

> imaketree' :: [(Tree,String)] -> [(Tree,String)]
> imaketree' x | iallrules x == iallrules ( iallrules (iallrules x)) = iallrules x
>              | otherwise = imaketree' (iallrules x)

> imaketree :: String -> [(Tree,String)]
> imaketree x = iloopid $ imaketree' (igentree (lines x))

END OF INCREMENTAL MAKETREE I

INCREMENTAL MAKETREE II

Okay, the idea for the incremental maketree function is to generalise the above so that each rule takes a list of trees as input and produces a list of trees as output. If the rule applies, it returns the list with the new tree appended to the end. If it doesn't apply it returns the old list unchanged. We might as well do this properly. We will make the list not just a list of trees, but a list of pairs of trees and strings. The string says what rule has been applied.  


> iicfc :: Tree -> IO (Tree)
> iicfc xs = do
>                clearScreen
>                setCursorPosition 0 0
>                if xs /= cfc xs 
>                    then do 
>                         putStrLn "applied the rule for closure...\n"
>                         putStrLn (printtree $ cfc xs)
>                         threadDelay mydelay
>                         return (cfc xs)
>                    else do
>                         return (xs)

> mydelay = 1000000

> iigentree :: [String] -> IO (Tree)
> iigentree xs = do
>                clearScreen
>                setCursorPosition 0 0
>                putStrLn "set up the tree...\n"
>                putStrLn (printtree $ gentree xs)
>                threadDelay mydelay
>                return (gentree xs)



Branching

> iidisj xs = (iiapplydisj xs) >>= iicfc
> iinegconj xs = (iiapplynegconj xs) >>= iicfc
> iicon xs = (iiapplycon xs) >>= iicfc
> iibicon xs = (iiapplybicon xs) >>= iicfc
> iinegbicon xs = (iiapplynegbicon xs) >>= iicfc


> iiapplydisj :: Tree -> IO (Tree)
> iiapplydisj xs = do
>                clearScreen
>                setCursorPosition 0 0
>                if xs /= applydisj xs 
>                    then do 
>                         putStrLn "applied the rule for disjunction...\n"
>                         putStrLn (printtree $ applydisj xs)
>                         threadDelay mydelay
>                         return (applydisj xs)
>                    else do
>                         return (xs)

> iiapplynegconj :: Tree -> IO (Tree)
> iiapplynegconj xs = do
>                clearScreen
>                setCursorPosition 0 0
>                if xs /= applynegconj xs 
>                    then do 
>                         putStrLn "applied the rule for negated conjunction...\n"
>                         putStrLn (printtree $ applynegconj xs)
>                         threadDelay mydelay
>                         return (applynegconj xs)
>                    else do
>                         return (xs)

> iiapplycon :: Tree -> IO (Tree)
> iiapplycon xs = do
>                clearScreen
>                setCursorPosition 0 0
>                if xs /= applycond xs 
>                    then do 
>                         putStrLn "applied the rule for conditional...\n"
>                         putStrLn (printtree $ applycond xs)
>                         threadDelay mydelay
>                         return (applycond xs)
>                    else do
>                         return (xs)

> iiapplybicon :: Tree -> IO (Tree)
> iiapplybicon xs = do
>                clearScreen
>                setCursorPosition 0 0
>                if xs /= applybicond xs 
>                    then do 
>                         putStrLn "applied the rule for biconditional...\n"
>                         putStrLn (printtree $ applybicond xs)
>                         threadDelay mydelay
>                         return (applybicond xs)
>                    else do
>                         return (xs)

> iiapplynegbicon :: Tree -> IO (Tree)
> iiapplynegbicon xs = do
>                clearScreen
>                setCursorPosition 0 0
>                if xs /= applynegbicond xs 
>                    then do 
>                         putStrLn "applied the rule for negated biconditional...\n"
>                         putStrLn (printtree $ applynegbicond xs)
>                         threadDelay mydelay
>                         return (applynegbicond xs)
>                    else do
>                         return (xs)

NonBranching

> iiconj xs = (iiapplyconj xs) >>= iicfc
> iinegdisj xs = (iiapplynegdisj xs) >>= iicfc
> iinegcon xs = (iiapplynegcon xs) >>= iicfc
> iinegsome xs = (iiapplynegsome xs) >>= iicfc
> iinegall xs = (iiapplynegall xs) >>= iicfc


> iiapplyconj :: Tree -> IO (Tree)
> iiapplyconj xs = do
>                clearScreen
>                setCursorPosition 0 0
>                if xs /= applyconj xs 
>                    then do 
>                         putStrLn "applied the rule for conjunction...\n"
>                         putStrLn (printtree $ applyconj xs)
>                         threadDelay mydelay
>                         return (applyconj xs)
>                    else do
>                         return (xs)

> iiapplynegdisj :: Tree -> IO (Tree)
> iiapplynegdisj xs = do
>                clearScreen
>                setCursorPosition 0 0
>                if xs /= applynegdisj xs 
>                    then do 
>                         putStrLn "applied the rule for negated disjunction...\n"
>                         putStrLn (printtree $ applynegdisj xs)
>                         threadDelay mydelay
>                         return (applynegdisj xs)
>                    else do
>                         return (xs)

> iiapplynegcon :: Tree -> IO (Tree)
> iiapplynegcon xs = do
>                clearScreen
>                setCursorPosition 0 0
>                if xs /= applynegcond xs 
>                    then do 
>                         putStrLn "applied the rule for negated conditional...\n"
>                         putStrLn (printtree $ applynegcond xs)
>                         threadDelay mydelay
>                         return (applynegcond xs)
>                    else do
>                         return (xs)

> iiapplynegsome :: Tree -> IO (Tree)
> iiapplynegsome xs = do
>                clearScreen
>                setCursorPosition 0 0
>                if xs /= applynegexe xs 
>                    then do 
>                         putStrLn "applied the rule for negated existential...\n"
>                         putStrLn (printtree $ applynegexe xs)
>                         threadDelay mydelay
>                         return (applynegexe xs)
>                    else do
>                         return (xs)

> iiapplynegall :: Tree -> IO (Tree)
> iiapplynegall xs = do
>                clearScreen
>                setCursorPosition 0 0
>                if xs /= applyneguni xs 
>                    then do 
>                         putStrLn "applied the rule for negated universal...\n"
>                         putStrLn (printtree $ applyneguni xs)
>                         threadDelay mydelay
>                         return (applyneguni xs)
>                    else do
>                         return (xs)

Double Negation

> iidneg xs = (iiapplydneg xs) >>= iicfc

> iiapplydneg :: Tree -> IO (Tree)
> iiapplydneg xs = do
>                clearScreen
>                setCursorPosition 0 0
>                if xs /= applydneg xs 
>                    then do 
>                         putStrLn "applied the rule for double negation...\n"
>                         putStrLn (printtree $ applydneg xs)
>                         threadDelay mydelay
>                         return (applydneg xs)
>                    else do
>                         return (xs)

Quantifiers

> iiexi xs = (iiapplyexi xs) >>= iicfc
> iiuni xs = (iiapplyuni xs) >>= iicfc

> iiapplyexi :: Tree -> IO (Tree)
> iiapplyexi xs = do
>                clearScreen
>                setCursorPosition 0 0
>                if xs /= applyexi xs 
>                    then do 
>                         putStrLn "applied the rule for existential quantifier...\n"
>                         putStrLn (printtree $ applyexi xs)
>                         threadDelay mydelay
>                         return (applyexi xs)
>                    else do
>                         return (xs)

> iiapplyuni :: Tree -> IO (Tree)
> iiapplyuni xs = do
>                clearScreen
>                setCursorPosition 0 0
>                if xs /= applyuni xs 
>                    then do 
>                         putStrLn "applied the rule for universal quantifier...\n"
>                         putStrLn (printtree $ applyuni xs)
>                         threadDelay mydelay
>                         return (applyuni xs)
>                    else do
>                         return (xs)

> iiloopdneg :: Tree -> IO (Tree) 
> iiloopdneg x = do
>                old <- iidneg x
>                new <- iidneg old
>                if old == new
>                    then return (new)
>                    else iiloopdneg new


> iiallnon xs = iiloopdneg xs >>= iiconj >>= iiloopdneg >>= iinegdisj >>= iiloopdneg >>= iinegcon >>= iiloopdneg >>= iinegsome >>= iiloopdneg >>= iinegall

> iiloopnon :: Tree -> IO (Tree)
> iiloopnon x = do
>               old <- iiallnon x
>               new <- iiallnon old
>               if old == new
>                    then return (new)
>                    else iiloopnon new


> iialls' xs = iiallnon xs >>= iidisj >>= iiallnon >>= iinegconj >>= iiallnon >>= iicon >>= iiallnon >>= iibicon >>= iiallnon >>= iinegbicon >>= iiallnon >>= iiexi >>= iiallnon


> iiidd x = (iiapplyid x) >>= iicfc

> iiapplyid :: Tree -> IO (Tree)
> iiapplyid xs = do
>                clearScreen
>                setCursorPosition 0 0
>                if xs /= applyid xs 
>                    then do 
>                         putStrLn "applied the substitution of identicals rule...\n"
>                         putStrLn (printtree $ applyid xs)
>                         threadDelay mydelay
>                         return (applyid xs)
>                    else do
>                         return (xs)

> iiloopid x = do
>              old <- iiidd x
>              new <- iiidd old
>              if old == new
>                   then return (new)
>                   else iiloopid new


> iiallrules xs = (iialls' xs) >>= iiuni

> makeloop x = do
>              old <- iiallrules x
>              new <- iiallrules old
>              if old == new
>                   then return (new)
>                   else makeloop new

> iimaketree :: String -> IO (Tree)
> iimaketree x = (iigentree (lines x)) >>= makeloop >>= iiloopid


END OF MAKETREE II




TREE STATS

> treestats :: Tree -> String
> treestats t = if null (getpaths t)
>               then "all paths close. not satisfiable."
>               else "not all paths close. satisfiable on the following model(s):\n" 

READ MODEL OFF TREE

The relevant unit here is a Path. We write everything to operate on a path and then we just map over paths.

> getnamesonpath :: Path -> String
> getnamesonpath p = sort $ nub $ concatMap getnames $ getpropsonpath p 

> temprefs :: Path -> [(Char,Int)]
> temprefs p = zip (getnamesonpath p) [1..]

> getids :: Path -> [(Char,Char)]
> getids p = concatMap getid (getpropsonpath p)
>     where getid (Atom (Pred2 'I') (x:y:[])) = [(x,y)]
>           getid _ = []

> keyval :: (Eq a) => a -> [(a,b)] -> b
> keyval x (y:ys) = if (fst y) == x
>     then snd y
>     else keyval x ys  



> changeval :: (Eq a) => a -> b -> [(a,b)] -> [(a,b)]
> changeval x z (y:ys) = if (fst y) == x
>     then (fst y,z) : changeval x z ys
>     else y : changeval x z ys  
> changeval x z [] = []

> trim :: [(Char,Int)] -> (Char,Char) -> [(Char,Int)]
> trim r (fst,snd) = changeval snd (keyval fst r) r 

> finalrefs :: Path -> [(Char,Int)]
> finalrefs p = foldl trim  (temprefs p)  (getids p)

> makedomain :: [(Char,Int)] -> [Int]
> makedomain xs = sort $ nub $ map dom xs
>     where dom (fst,snd) = snd    

> namestorefs :: String -> [(Char,Int)] -> [Int]
> namestorefs xs r = map (nametoref r) xs
>     where nametoref r x = keyval x r 

> makeonepreds :: Path -> [(Char,[Int])]
> makeonepreds p = nub $ concatMap onepred (getpropsonpath p)
>     where onepred (Atom (Pred1 x) y) = [(x,(namestorefs y (finalrefs p)))] 
>           onepred _ = []


> maketwopreds :: Path -> [(Char,[(Int,Int)])]
> maketwopreds p = nub $ concatMap twopred (getpropsonpath p)
>     where twopred (Atom (Pred2 'I') y) = []
>           twopred (Neg (Atom (Pred2 'I') y)) = []
>           twopred (Atom (Pred2 x) y) = [(x,[topairs (namestorefs y (finalrefs p))])] 
>           twopred _ = []
>           topairs (x:y:[]) = (x,y)

> makethreepreds :: Path -> [(Char,[(Int,Int,Int)])]
> makethreepreds p = nub $ concatMap threepred (getpropsonpath p)
>     where threepred (Atom (Pred3 x) y) = [(x,[totriples (namestorefs y (finalrefs p))])] 
>           threepred _ = []
>           totriples (x:y:z:[]) = (x,y,z)

> keyval' :: (Eq a) => a -> [(a,b)] -> [b]
> keyval' x (y:ys) = if (fst y) == x
>     then [snd y] ++ keyval' x ys
>     else keyval' x ys  
> keyval' x [] = []

> getall :: [(Char,[a])] -> Char -> (Char,[a])
> getall xs x = (x,(concat (keyval' x xs)))

> getall' :: [Char] -> [(Char,[a])] -> [(Char,[a])]
> getall' ys  xs = map (getall xs) ys

> getpredlet1 :: Path -> [Char]
> getpredlet1 p = nub $ concatMap getpredicates1 (getpropsonpath p)

> getpredlet2 :: Path -> [Char]
> getpredlet2 p = nub $ concatMap getpredicates2' (getpropsonpath p)

> getpredlet3 :: Path -> [Char]
> getpredlet3 p = nub $ concatMap getpredicates3 (getpropsonpath p)

> onepreds :: Path -> [(Char,[Int])] 
> onepreds p = nub $ getall' (getpredlet1 p) (makeonepreds p)  

> twopreds :: Path -> [(Char,[(Int,Int)])] 
> twopreds p = nub $ getall' (getpredlet2 p) (maketwopreds p)  

> threepreds :: Path -> [(Char,[(Int,Int,Int)])] 
> threepreds p = nub $ getall' (getpredlet3 p) (makethreepreds p)  

> readmodel :: Path -> Model
> readmodel p = Model  (makedomain (finalrefs p)) (finalrefs p) (onepreds p) (twopreds p) (threepreds p)  

> readmodels :: Tree -> [Model]
> readmodels t = map readmodel (getpaths t)     
 
PREPARE INPUT

> gentree :: [String] -> Tree
> gentree xs = Branch (gentest xs) []

> gentest :: [String] -> [Elem]
> gentest xs = map f xs
>     where f x = proptoelem ((parser x)!!0)  

> proptoelem :: Prop -> Elem
> proptoelem p = Elem p [] False  
