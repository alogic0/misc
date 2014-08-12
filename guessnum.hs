module Main where
import System.IO
import System.Random
import Text.Read

main = do
    hidNum <- randomRIO (1::Int, 100)
    putStrLn "I thought a number between 1 and 100"
    putStr "Try to guess it: "
    guess 1 hidNum

guess :: Int -> Int -> IO ()
guess steps hidNum = do
    inp <- getLine
    let uNum = (read inp) :: Int
    if uNum > hidNum
    then do 
        putStr $ "Try less: "
        guess (steps + 1) hidNum
    else if uNum < hidNum
         then do
            putStr $ "Try more: "
            guess (steps+1) hidNum
         else putStrLn $ "Congratulations! You won in " 
                         ++ (show steps) ++ " steps."
