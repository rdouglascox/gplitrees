> module GPLIparser (parser, Prop (..), Pred(..), getpredicates1, getpredicates2, getpredicates2', getpredicates3, getnames, substitute, rmdups) where

> import Control.Monad (liftM, ap)
> import Data.Char
> import Data.List
> import DataProp

-- parser function to export

> parser :: String -> [Prop]
> parser x = if null (apply prop x) ||not  (null (snd (head (apply prop x))))
>            then [] 
>            else [fst $ head $ apply prop x] 

-- general parser functions

> newtype Parser a = Parser (String -> [(a,String)])
> apply :: Parser a -> String -> [(a,String)]
> apply (Parser p) s = p s

> parse :: Parser a -> String -> a
> parse p = fst . head . apply p

--- type declaration

> instance Monad Parser where
>    return x = Parser (\s -> [(x,s)])
>    p >>= q  = Parser (\s -> [(y,s2)
>                            | (x,s') <- apply p s,
>                              (y,s2) <- apply (q x) s'])

> instance Functor Parser where
>    fmap = liftM

> instance Applicative Parser where
>     pure = return
>     (<*>) = ap

> getc :: Parser Char
> getc = Parser f
>        where f [] = []
>              f (c:cs) = [(c,cs)]

> sat :: (Char -> Bool) -> Parser Char
> sat p = do {c <- getc;
>            if p c then return c
>            else ffail}

> ffail = Parser (\s -> [])

> char :: Char -> Parser ()
> char x = do {c <- sat (==x); return ()}

> string :: String -> Parser ()
> string [] = return ()
> string (x:xs) = do {char x; string xs; return ()}

--- choice operator for parsers

> (<|>) :: Parser a -> Parser a -> Parser a
> p <|> q = Parser f
>           where f s = let ps = apply p s in 
>                       if null ps then apply q s
>                       else ps



> atomic' :: Parser Prop
> atomic' = do x <- preds
>              y <- term
>              return (Atom (Pred1 x) [y])

> atomic2 :: Parser Prop
> atomic2 = do x <- preds
>              y <- term
>              z <- term
>              return (Atom (Pred2 x) [y,z])

> atomic3 :: Parser Prop
> atomic3 = do x <- preds
>              y <- term
>              z <- term
>              u <- term
>              return (Atom (Pred3 x) [y,z,u])

> preds :: Parser Char
> preds = do c <- sat (`elem` ['A'..'Z'])
>            return (c)


> variables :: String
> variables = ['u','w','y','x','z'] 

> names :: String 
> names = ['a'..'t']

> terms :: String
> terms = variables ++ names

> term :: Parser Char
> term = do c <- sat (`elem` terms)
>           return (c)

> neg :: Parser Prop
> neg = do c <- sat (=='~')
>          x <- prop 
>          return (Neg x)

> conj :: Parser Prop
> conj = do x <- parens
>           y <- prop
>           z <- conj'
>           u <- prop
>           t <- parens
>           return (Conj y u)  

> disj :: Parser Prop
> disj = do x <- parens
>           y <- prop
>           z <- disj'
>           u <- prop
>           t <- parens
>           return (Disj y u)  

> cond :: Parser Prop
> cond = do x <- parens
>           y <- prop
>           z <- cond'
>           u <- prop
>           t <- parens
>           return (Cond y u)  

> bicon :: Parser Prop
> bicon = do x <- parens
>            y <- prop
>            z <- bicon'
>            u <- prop
>            t <- parens
>            return (Bicon y u)  

> parens :: Parser ()
> parens = do c <- sat (=='(') 
>             return () 
>         <|> do c <- sat (==')')
>                return ()

> conj' :: Parser ()
> conj' = do c <- string' "&"
>            return ()

> disj' :: Parser () 
> disj' = do c <- string'  "v"
>            return ()

> cond' :: Parser ()
> cond' = do c <- string' "->" 
>            return ()

> bicon' :: Parser ()
> bicon' = do c <- string' "<->"
>             return ()

> string' :: String -> Parser String
> string' [] = return []
> string' (x:xs) = do char x
>                     string' xs
>                     return (x:xs)

> exi :: Parser Prop
> exi = do x <- sat (== '#') 
>          c <- sat (`elem` variables)
>          p <- prop
>          return (Exi c p)
 
> uni :: Parser Prop
> uni = do x <- sat (== '@') 
>          c <- sat (`elem` variables)
>          p <- prop
>          return (Uni c p)

> iden :: Parser Prop
> iden = do x <- sat (`elem` terms)
>           y <- sat (=='=')
>           z <- sat (`elem` terms)
>           return (Atom (Pred2 'I') [x,z])

> niden :: Parser Prop
> niden = do x <- sat (`elem` terms)
>            y <- string' "/="
>            z <- sat (`elem` terms)
>            return (Neg (Atom (Pred2 'I') [x,z]))

> prop :: Parser Prop
> prop = do x <- atomic3 
>           return (x)
>        <|> do x <- atomic2
>               return (x)
>        <|> do x <- atomic'
>               return (x)
>        <|> do x <- neg
>               return (x)
>        <|> do x <- niden
>               return (x)
>        <|> do x <- iden 
>               return (x)
>        <|> do x <- conj
>               return (x)
>        <|> do x <- disj
>               return (x)         
>        <|> do x <- bicon
>               return (x)
>        <|> do x <- cond
>               return (x)
>        <|> do x <- uni
>               return (x)
>        <|> do x <- exi
>               return (x)


END PARSER

Some helper functions.

> getpredicates1 :: Prop -> String
> getpredicates1 (Neg x) = nub $ getpredicates1 x
> getpredicates1 (Disj x y) = nub $ (getpredicates1 x) ++ (getpredicates1 y)
> getpredicates1 (Conj x y) = nub $ (getpredicates1 x) ++ (getpredicates1 y)
> getpredicates1 (Cond x y) = nub $ (getpredicates1 x) ++ (getpredicates1 y)
> getpredicates1 (Bicon x y) = nub $ (getpredicates1 x) ++ (getpredicates1 y)
> getpredicates1 (Exi x y) = nub $ (getpredicates1 y)
> getpredicates1 (Uni x y) = nub $ (getpredicates1 y)
> getpredicates1 (Atom (Pred1 x) y) = [x]
> getpredicates1 (Atom (Pred2 x) y) = []
> getpredicates1 (Atom (Pred3 x) y) = []


> getpredicates2 :: Prop -> String
> getpredicates2 (Neg x) = nub $ getpredicates2 x
> getpredicates2 (Disj x y) = nub $ (getpredicates2 x) ++ (getpredicates2 y)
> getpredicates2 (Conj x y) = nub $ (getpredicates2 x) ++ (getpredicates2 y)
> getpredicates2 (Cond x y) = nub $ (getpredicates2 x) ++ (getpredicates2 y)
> getpredicates2 (Bicon x y) = nub $ (getpredicates2 x) ++ (getpredicates2 y)
> getpredicates2 (Exi x y) = nub $ (getpredicates2 y)
> getpredicates2 (Uni x y) = nub $ (getpredicates2 y)
> getpredicates2 (Atom (Pred2 x) y) = [x]
> getpredicates2 (Atom (Pred1 x) y) = []
> getpredicates2 (Atom (Pred3 x) y) = []

> getpredicates2' :: Prop -> String
> getpredicates2' (Neg (Atom (Pred2 'I') y)) = []
> getpredicates2' (Neg x) = nub $ getpredicates2' x
> getpredicates2' (Disj x y) = nub $ (getpredicates2' x) ++ (getpredicates2' y)
> getpredicates2' (Conj x y) = nub $ (getpredicates2' x) ++ (getpredicates2' y)
> getpredicates2' (Cond x y) = nub $ (getpredicates2' x) ++ (getpredicates2' y)
> getpredicates2' (Bicon x y) = nub $ (getpredicates2' x) ++ (getpredicates2' y)
> getpredicates2' (Exi x y) = nub $ (getpredicates2' y)
> getpredicates2' (Uni x y) = nub $ (getpredicates2' y)
> getpredicates2' (Atom (Pred2 'I') y) = []
> getpredicates2' (Atom (Pred2 x) y) = [x]
> getpredicates2' (Atom (Pred1 x) y) = []
> getpredicates2' (Atom (Pred3 x) y) = []




> getpredicates3 :: Prop -> String
> getpredicates3 (Neg x) = nub $ getpredicates3 x
> getpredicates3 (Disj x y) = nub $ (getpredicates3 x) ++ (getpredicates3 y)
> getpredicates3 (Conj x y) = nub $ (getpredicates3 x) ++ (getpredicates3 y)
> getpredicates3 (Cond x y) = nub $ (getpredicates3 x) ++ (getpredicates3 y)
> getpredicates3 (Bicon x y) = nub $ (getpredicates3 x) ++ (getpredicates3 y)
> getpredicates3 (Exi x y) = nub $ (getpredicates3 y)
> getpredicates3 (Uni x y) = nub $(getpredicates3 y)
> getpredicates3 (Atom (Pred3 x) y) = [x]
> getpredicates3 (Atom (Pred1 x) y) = []
> getpredicates3 (Atom (Pred2 x) y) = []

> getnames :: Prop -> String
> getnames (Neg x) = nub $ getnames x
> getnames (Disj x y) = nub $ (getnames x) ++ (getnames y)
> getnames (Conj x y) = nub $ (getnames x) ++ (getnames y)
> getnames (Cond x y) = nub $ (getnames x) ++ (getnames y)
> getnames (Bicon x y) = nub $ (getnames x) ++ (getnames y)
> getnames (Exi x y) = nub $ (getnames y)
> getnames (Uni x y) = nub $ (getnames y)
> getnames (Atom (Pred1 x) y) = filter (`elem` ['a'..'t']) y
> getnames (Atom (Pred2 x) y) = filter (`elem` ['a'..'t']) y
> getnames (Atom (Pred3 x) y) = filter (`elem` ['a'..'t']) y
 

> substitute :: Prop -> Char -> Char -> Prop
> substitute (Neg x) target sub= Neg (substitute x target sub)
> substitute (Disj x y) target sub = Disj (substitute x target sub) (substitute y target sub)
> substitute (Conj x y) target sub= Conj (substitute x target sub) (substitute y target sub)
> substitute (Cond x y) target sub= Cond (substitute x target sub) (substitute y target sub)
> substitute (Bicon x y) target sub= Bicon (substitute x target sub) (substitute y target sub)
> substitute (Exi x y) target sub= if x == target
>     then Exi x y
>     else Exi x (substitute y target sub)
> substitute (Uni x y) target sub= if x == target 
>     then Uni x y
>     else Uni x (substitute y target sub)
> substitute (Atom (Pred1 x) y) target sub =  Atom (Pred1 x) (map (suby target sub) y)
> substitute (Atom (Pred2 x) y) target sub = Atom (Pred2 x) (map (suby target sub) y)
> substitute (Atom (Pred3 x) y) target sub = Atom (Pred3 x) (map (suby target sub) y)

> suby x y z = if x == z  then y else z  


> rmdups :: Ord a => [a] -> [a]
> rmdups = map head . group . sort






