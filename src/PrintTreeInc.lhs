> module PrintTreeInc (printtreeinc',printtreeinc'',select) where

> import System.Environment
> import MakeTree
> import System.Console.ANSI
> import GPLIprinter
> import Control.Concurrent 
> import PrintModels
> import Data.List
> import GPLIevaluator
> import GPLIenumerator
> import LatexTrees
> import Control.Concurrent

> select = do
>          x <- getContents
>          args <- getArgs
>          selecthelper x args


> selecthelper x args | ("-i" `elem` args) || ("--incremental" `elem` args) = printtreeinc' x args
>                     | ("-r" `elem` args) || ("--race" `elem` args) = printtreerace x args
>                     | otherwise = printtreeinc'' x args


> printtreeinc'' x args = do
>                       let tree = maketree x
>                       putStrLn ("input:\n\n" ++ x)
>                       putStrLn ("tree:\n\n" ++ printtree tree)
>                       putStrLn (readtree tree)
>                       let models = readmodels tree
>                       if ("-m" `elem` args) || ("--models" `elem` args) 
>                           then do
>                                putStrLn "\nsatisfiable on the following model(s):\n"
>                                mapM_ putStrLn $ intersperse "" $ map printmodel models
>                       else return ()
>                       if (("-v" `elem` args) || ("--verify" `elem` args))  && not (null models)
>                           then verifymodels x models
>                           else putStr ""
>                       if not (null models) && (("-n" `elem` args) || ("--number" `elem` args))
>                           then do putStrLn "\nand here are some more models...\n"
>                                   mapM_ putStrLn $ intersperse "" $ map printmodel (getmoremodels (prepinput x) (processargs2 args)) 
>                           else return ()
>                       putStrLn ""
>                       if ("-s" `elem` args) || ("--stats" `elem` args)
>                           then putStrLn ((treestats tree) ++ "\n")
>                           else return ()
>                       if ("-l" `elem` args) || ("--latex" `elem` args)
>                           then putStrLn (treetolatex tree)
>                           else return () 
>                       if ("-w" `elem` args) || ("--write" `elem` args)
>                           then do treewriter x tree models
>                           else return ()
>                       putStrLn "done!"

> printtreeinc' x args = do
>                       putStrLn ("input:\n\n" ++ x)
>                       threadDelay 2000000
>                       tree <- iimaketree x
>                       putStrLn ("finished tree...\n\n" ++ printtree tree)
>                       putStrLn "\n"
>                       cursorUp 2
>                       putStrLn (readtree tree)
>                       let models = readmodels tree
>                       if ("-m" `elem` args) || ("--models" `elem` args) 
>                           then do
>                                putStrLn "\nsatisfiable on the following model(s):\n"
>                                mapM_ putStrLn $ intersperse "" $ map printmodel models
>                       else return ()
>                       if (("-v" `elem` args) || ("--verify" `elem` args))  && not (null models)
>                           then verifymodels x models
>                           else putStr ""
>                       if not (null models) && (("-n" `elem` args) || ("--number" `elem` args))
>                           then do putStrLn "\nand here are some more models...\n"
>                                   mapM_ putStrLn $ intersperse "" $ map printmodel (getmoremodels (prepinput x) (processargs2 args)) 
>                           else return ()
>                       putStrLn ""
>                       if ("-s" `elem` args) || ("--stats" `elem` args)
>                           then putStrLn ((treestats tree) ++ "\n")
>                           else return ()
>                       if ("-l" `elem` args) || ("--latex" `elem` args)
>                           then putStrLn (treetolatex tree)
>                           else return () 
>                       if ("-w" `elem` args) || ("--write" `elem` args)
>                           then do treewriter x tree models
>                           else return ()
>                       putStrLn "done!"


> printtreerace x args = do
>                        let delay = processargs3 args
>                        z <- forkIO (satm x)
>                        t <- forkIO (printtreerace' x)
>                        threadDelay delay
>                        return ()

> printtreerace' x = do
>                    let tree = maketree x
>                    putStrLn (readtree' tree)


> printtreeinc x args = do
>                let delay = processargs args 
>                tree <- iimaketree x
>                putStrLn ("finished tree...\n\n" ++ printtree tree)
>                putStr "\n"
>                cursorUp 2
>                putStrLn ("\n" ++ (readtree tree))
>                threadDelay (delay * 3) 
>                let models = readmodels tree
>                mapM_ putStrLn $ intersperse "" $ map printmodel models
>                if ("-v" `elem` args) && not (null models)
>                    then verifymodels x models
>                    else putStr ""
>                if (null models) || (not ("-n" `elem` args))
>                    then do putStr ""
>                    else do putStrLn "\nand here are some more models...\n"
>                            threadDelay (delay * 3) 
>                            mapM_ putStrLn $ intersperse "" $ map printmodel (getmoremodels (prepinput x) (processargs2 args)) 
>                putStrLn ""
>                threadDelay (delay * 3) 
>                putStrLn "bye!"


> verifymodels :: String -> [Model] -> IO ()
> verifymodels xs ms = do if (and (map (val' (prepinput xs)) ms))
>                             then do 
>                                 putStrLn "\nverifying models...\n"
>                                 setSGR [SetColor Foreground Vivid Green]
>                                 putStrLn "    all good!"
>                                 setSGR [Reset]
>                             else do 
>                                 putStrLn "\nverifying models...\n"    
>                                 setSGR [SetColor Foreground Vivid Red]
>                                 putStrLn "    error!"
>                                 setSGR [Reset]


> getmoremodels :: String -> Int -> [Model]
> getmoremodels xs n = take n $ filter (val' xs) (superenum xs)  

> satmodels :: String -> Bool
> satmodels xs = or  (map (val' xs) (superenum xs))

> satm :: String -> IO ()
> satm x = if satmodels (prepinput x)
>              then do putStrLn "satisfiable! (by models method)" 
>              else putStrLn "not satisfiable! (by models method)"  

> processargs :: [String] -> Int
> processargs (x:y:ys) = if x == "-d" 
>                        then (100000 * (read y :: Int))
>                        else processargs (y:ys)
> processargs (x:[]) = 0
> processargs [] = 0 

> processargs3 :: [String] -> Int
> processargs3 (x:y:ys) = if (x == "-r") || (x == "--race") 
>                        then (1000000 * (read y :: Int))
>                        else processargs3 (y:ys)
> processargs3 (x:[]) = 0
> processargs3 [] = 0 



> processargs2 :: [String] -> Int
> processargs2 (x:y:ys) = if (x == "-n") || (x == "--number")
>                        then (read y :: Int)
>                        else processargs2 (y:ys)
> processargs2 (x:[]) = 0
> processargs2 [] = 0 



> prepinput :: String -> String
> prepinput x = if length (lines x) == 1
>     then concat $ lines x
>     else conjoin (lines x)


> conjoin :: [String] -> String
> conjoin (x:[]) = x
> conjoin (x:ys) = "(" ++ x ++ "&" ++ conjoin ys ++ ")" 

> val' :: String -> Model -> Bool
> val' x y = val y (genassignment y x) (head (parser x))  



> printdelay y z x = do
>                    clearScreen
>                    setCursorPosition 0 0 
>                    putStrLn ("input:\n\n" ++ y)
>                    putStrLn x
>                    threadDelay z 

> printitree (x,y) = y ++ "\n\n" ++ (printtree x) ++ "\n\n"
