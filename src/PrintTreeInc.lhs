> module PrintTreeInc where

> import System.Environment
> import MakeTree
> import System.Console.ANSI
> import GPLIprinter
> import Control.Concurrent 
> import PrintModels
> import Data.List
> import GPLIevaluator
> import GPLIenumerator

> printtreeinc = do
>                x <- getContents
>                args <- getArgs
>                let delay = processargs args 
>                let tree = imaketree x
>                mapM_ (printdelay x delay) ((map printitree (tree)) ++ ["finished tree...\n\n" ++ (printtree (fst (last tree))) ++ "\n"]  )
>                cursorUp 2
>                putStrLn ("\n" ++ (treestats (fst (last tree))))
>                threadDelay (delay * 3) 
>                let models = readmodels (fst (last tree))
>                mapM_ putStrLn $ intersperse "" $ map printmodel models
>                if "-v" `elem` args
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
> verifymodels xs ms = do if null ms 
>                         then do putStrLn ""
>                         else do if (and (map (val' (prepinput xs)) ms))
>                                 then do 
>                                      putStrLn "\nverifying models...\n"
>                                      setSGR [SetColor Foreground Vivid Green]
>                                      putStrLn "    all good!"
>                                      setSGR [Reset]
>                                 else do 
>                                      putStrLn "\nverifying models...\n"    
>                                      setSGR [SetColor Foreground Vivid Red]
>                                      putStrLn "    error!"
>                                      setSGR [Reset]

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
>     then concat (intersperse "&" (lines x))
>     else "(" ++ (concat (intersperse "&" (lines x)))  ++ ")"

> val' :: String -> Model -> Bool
> val' x y = val y (genassignment y x) (head (parser x))  



> printdelay y z x = do
>                    clearScreen
>                    setCursorPosition 0 0 
>                    putStrLn ("input:\n\n" ++ y)
>                    putStrLn x
>                    threadDelay z 

> printitree (x,y) = y ++ "\n\n" ++ (printtree x) ++ "\n\n"
