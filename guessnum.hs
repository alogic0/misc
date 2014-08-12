module Main where
import System.IO
import System.Random
import Text.Read
import Data.Maybe

main = do
    let a=1::Int
        b=100::Int
    hidNum <- randomRIO (a, b)
    putStrLn $ "I thought a number between " ++ show a ++ " and " ++ show b
    putStrLn "Try to guess it!"
    guess hidNum a b 1

askUser :: Int -> Int -> IO Int
askUser a b = do
    putStr $ "Put a number between " ++ show a ++ " and " ++ show b ++ ": "
    inp <- getLine
    case (readMaybe inp) of
      Nothing -> askUser a b
      Just n -> return n

guess :: Int -> Int -> Int -> Int -> IO ()
guess hidNum a b steps = do
    uNum <- askUser a b
    if uNum > hidNum
    then do
        putStrLn "Mine is lesser!"
        guess hidNum a uNum (steps + 1)
    else if uNum < hidNum
         then do
            putStrLn "Mine is bigger!"
            guess hidNum uNum b (steps + 1)
         else putStrLn $ "Congratulations! You won in " 
                         ++ (show steps) ++ " steps."
