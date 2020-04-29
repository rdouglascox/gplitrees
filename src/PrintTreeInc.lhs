> module PrintTreeInc (printtreeinc, printtreeinc',printtreeinc'',select) where

> import System.Environment
> import MakeTree
> import System.Console.ANSI
> import GPLIprinter
> import Control.Concurrent 
> import PrintModels
> import Data.List
> import GPLIevaluator
> import GPLIenumerator

> select = do
>          x <- getContents
>          args <- getArgs
>          selecthelper x args


> selecthelper x args | "-i" `elem` args = printtreeinc' x args
>                     | otherwise = printtreeinc'' x args


> printtreeinc'' x args = do
>                       let tree = maketree x
>                       putStrLn ("finished tree...\n\n" ++ printtree tree)
>                       putStr "\n"
>                       putStrLn ("\n" ++ (treestats tree))
>                       let models = readmodels tree
>                       mapM_ putStrLn $ intersperse "" $ map printmodel models
>                       if ("-v" `elem` args) && not (null models)
>                           then verifymodels x models
>                           else putStr ""
>                       if (null models) || (not ("-n" `elem` args))
>                           then do putStr ""
>                           else do putStrLn "\nand here are some more models...\n"
>                                   mapM_ putStrLn $ intersperse "" $ map printmodel (getmoremodels (prepinput x) (processargs2 args)) 
>                       putStrLn ""
>                       putStrLn "bye!"

> printtreeinc' x args = do
>                let delay = processargs args 
>                tree <- iimaketree x
>                putStrLn ("finished tree...\n\n" ++ printtree tree)
>                putStr "\n"
>                threadDelay 6000000
>                cursorUp 2
>                putStrLn ("\n" ++ (treestats tree))
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



> printtreeinc x args = do
>                let delay = processargs args 
>                let tree = imaketree x
>                mapM_ (printdelay x delay) ((map printitree (tree)) ++ ["finished tree...\n\n" ++ (printtree (fst (last tree))) ++ "\n"]  )
>                cursorUp 2
>                putStrLn ("\n" ++ (treestats (fst (last tree))))
>                threadDelay (delay * 3) 
>                let models = readmodels (fst (last tree))
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


> processargs :: [String] -> Int
> processargs (x:y:ys) = if x == "-d" 
>                        then (100000 * (read y :: Int))
>                        else processargs (y:ys)
> processargs (x:[]) = 0
> processargs [] = 0 

> processargs2 :: [String] -> Int
> processargs2 (x:y:ys) = if x == "-n" 
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
