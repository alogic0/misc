module Justify where
import Data.List

type Word = String

takeLine :: Int -> [Word] -> [Word] -> ([Word],[Word])
takeLine _ acc [] = (acc,[])
takeLine n acc xs@(x:xs')
  | lenX <= n = takeLine (n - lenX - 1)  acc' xs'
  | otherwise = (acc, xs)
  where 
    acc' = acc ++ [x]
    lenX = length x

justifyLine :: Int -> [Word] -> String
justifyLine lineLen ws@(_:_:_)=
  let nGaps = length ws - 1
      diff = lineLen - length (concat ws)
      (nSp, bGaps) = diff `divMod` nGaps
      spList = replicate bGaps (replicate (nSp + 1) ' ')
               ++ replicate (nGaps - bGaps) (replicate nSp ' ')
      wL = last ws
  in
    concat [w ++ ss | (w,ss) <- zip ws spList] ++ wL
justifyLine _ ws = concat ws

splitLines :: Int -> [Word] -> [String]
splitLines lineLen ws =
  case takeLine lineLen [] ws of
    (xss, []) -> [unwords xss]
    (xss, ws') -> ((justifyLine lineLen xss) ++ "\n") : splitLines lineLen ws'

justify :: String -> Int -> String
justify text width = concat $ splitLines width $ words text
