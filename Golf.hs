module Golf where

import Data.List (transpose)
--import System.Random
--putStr $ histogram $ take 60 $ randomRs (0,9) (mkStdGen 0)

skipN :: [a] -> Int -> [a]
skipN xs n =
  let ys' = drop (n-1) xs
  in case ys' of
        (y:ys) -> y : skipN ys n
        [] -> []

skips :: [a] -> [[a]]
skips xs = map (skipN xs) [1 .. length xs]

histogram :: [Integer] -> String
histogram xs =
  let ns = map (\n -> length $ filter (== n) xs) [0 .. 9]
      maxLen = maximum ns
      strSpace = replicate maxLen ' '
      xss = map (\n -> reverse $ take maxLen $ (++ strSpace) $ replicate n '*') ns
      xs' = concatMap (++ "\n") $ transpose xss 
  in xs' ++ replicate 10 '=' ++ "\n" ++ ['0' .. '9'] ++ "\n"
          
