> module LatexTrees (treetolatex, treewriter) where

> import DataProp
> import DataTree
> import Data.List
> import Data.Time
> import System.IO
> import GPLIevaluator
> import Data.List

> treewriter i t ms = do
>                putStrLn "write to file y/n?\n "
>                tty <- openFile "/dev/tty" ReadMode
>                ans <- hGetLine tty
>                if ans == "y"
>                    then do       
>                       time <- getZonedTime
>                       let ftime = formatTime defaultTimeLocale "%F_%T" time
>                       let filename = "textree_" ++ ftime ++ ".tex"
>                       writeFile filename (template i t ms)
>                       putStrLn ("\nwrote tree to file: " ++ filename ++ "\n")
>                    else putStrLn ""
>                hClose tty

> template :: String -> Tree -> [Model] -> String
> template i t ms = "\\documentclass[12pt,a4paper]{article}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amsmath}\n\\usepackage{amsfonts}\n\\usepackage{amssymb}\n\\usepackage{pifont}\n\\usepackage{qtree}\n\\usepackage[left=2cm,right=2cm,top=2cm,bottom=2cm]{geometry}\n\\begin{document}\n\\small\n\\noindent\ninput:\\\\\\\n\n\\noindent\n" ++ inputtolatex i ++ "\n\\\\\n\n\\noindent\ntree:\\\\\\\n\n\\noindent\\Tree " ++ treetolatex t ++ "\\\\\n\n\\noindent\nmodels:\\\\\n\n\\noindent\n" ++ modelstolatex ms ++ "\n\\end{document}"   


> proptolatex :: Prop -> String
> proptolatex (Atom (Pred1 c) xs) = c : xs
> proptolatex (Atom (Pred2 'I') (x:y:[])) = [x] ++ "$=$" ++ [y]   
> proptolatex (Atom (Pred2 c) xs) = c : xs 
> proptolatex (Atom (Pred3 c) xs) = c : xs 
> proptolatex (Neg p) = "$\\lnot$" ++ proptolatex p 
> proptolatex (Conj l r) = "(" ++ proptolatex l ++ "$\\land$"   ++ proptolatex r ++ ")"
> proptolatex (Disj l r) = "(" ++ proptolatex l ++ "$\\lor$"  ++ proptolatex r ++ ")"
> proptolatex (Cond l r) = "(" ++ proptolatex l ++ "$\\rightarrow$"  ++ proptolatex r ++ ")"
> proptolatex (Bicon l r) = "(" ++ proptolatex l ++ "$\\leftrightarrow$"  ++ proptolatex r ++ ")"
> proptolatex (Exi c p) = "$\\exists$" ++ [c] ++ proptolatex p
> proptolatex (Uni c p) = "$\\forall$" ++ [c] ++ proptolatex p

> printelem :: Elem -> String
> printelem (Elem (Exi c p) sub True) = (proptolatex (Exi c p)) ++ "\\checkmark " ++ (ifsub' sub) 
> printelem (Elem prop sub True) = (proptolatex prop) ++ (ifsub sub) ++ "\\checkmark"
> printelem (Elem prop sub False) = (proptolatex prop) ++ (ifsub sub)

> ifsub x = if not (null x)
>           then (" \\textbackslash " ++ x)
>           else [] 

> ifsub' x = if not (null x)
>           then (x)
>           else [] 



> printelems :: [Elem] -> String
> printelems xs = concat (intersperse "\\\\" (map printelem xs))

> treetolatex :: Tree -> String
> treetolatex (Branch es [(Branch [] [])]) = "[.{" ++ printelems es ++ "\\\\x} ]" 
> treetolatex (Branch es []) = "[.{" ++ printelems es ++ "\\\\o} ]"  
> treetolatex (Branch es ts) = "[.{" ++ printelems es ++ "} " ++ concatMap treetolatex ts ++ " ]"  

> tolatex :: [Char] -> [Char]
> tolatex xs | null xs = []
>            | (take 3 xs) == "<->" = "$\\leftrightarrow$" ++ tolatex (drop 3 xs)
>            | (take 2 xs) == "->" = "$\\rightarrow$" ++ tolatex (drop 2 xs)
>            | (take 1 xs) == "&" = "$\\land$" ++ tolatex (drop 1 xs)
>            | (take 1 xs) == "v" = "$\\lor$" ++ tolatex (drop 1 xs)
>            | (take 1 xs) == "~" = "$\\lnot$" ++ tolatex (drop 1 xs)
>            | (take 1 xs) == "@" = "$\\forall$" ++ tolatex (drop 1 xs)
>            | (take 1 xs) == "#" = "$\\exists$" ++ tolatex (drop 1 xs)
>            | otherwise = take 1 xs ++ tolatex (drop 1 xs)

> inputtolatex :: String -> String
> inputtolatex xs = concat (intersperse "\\\\" (lines (tolatex xs)))

> modelstolatex :: [Model] -> String 
> modelstolatex ms = concat $ intersperse "\n\n\\noindent\n" (map printmodel ms)

> printmodel :: Model -> String
> printmodel m = "model:\\\\" ++ "\n\\hspace*{1em}  domain: " ++ printdom m ++ printrefs m ++ "\\\\\n\\hspace*{1em}  extensions:\\\\" ++ printextensions m

> printdom :: Model -> String
> printdom m = show (domain m)

> printrefs :: Model -> String
> printrefs m = if not (null (referents m))
>               then ("\\\\\n\\hspace*{1em}  referents: \\\\\n\\hspace*{3em} " ++ (printpairs' (referents m)))
>               else ""

> pext1 :: Model -> String
> pext1 m = if not (null (extensions' m))
>           then ("\n\\hspace*{2em}    one-place: \\\\\n\\hspace*{4em} " ++ (printpairs (extensions' m)))
>           else ""

> pext2 :: Model -> String
> pext2 m = if not (null (extensions'' m))
>           then ("\n\\hspace*{2em}    two-place: \\\\\n\\hspace*{4em} " ++ (printpairs (extensions'' m)))
>           else ""

> pext3 :: Model -> String
> pext3 m = if not (null (extensions''' m))
>           then ("\n\\hspace*{2em}    three-place: \\\\\n\\hspace*{4em} " ++ (printpairs (extensions''' m)))
>           else ""

> printpairs xs = concat $ intersperse "\n\\hspace*{4em} " (map pairs xs)
>     where pairs x = [fst x] ++ ": " ++ show (snd x) ++ "\\\\"

> printpairs' xs = concat $ intersperse " \\\\\n\\hspace*{3em} " (map pairs xs)
>     where pairs x = [fst x] ++ ": " ++ show (snd x)

> printextensions m = pext1 m ++ pext2 m ++ pext3 m

