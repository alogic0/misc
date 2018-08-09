module Main where

import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import System.Environment
import Control.Monad.State

-- name example: "4_CAM 6_main_20180801101345_20180801101432.avi"

infixr 5 +++
(+++) :: RE Char String -> RE Char String -> RE Char String
(+++) = liftA2 (++)

dt = concatMap show <$> many digit

pt1 = foldr1 (+++) [dt
                   , string "_"
                   , string "C" <|> string "K"
                   , string "AM"
                   , const "_" <$> string " "
                   , dt <* string "_main_"]

pt2 = dt +++ string "_" +++ dt

{-
rn s = do
   (p1, rs) <- findFirstPrefix  pt1 s
   (p2, rs2) <- findFirstPrefix pt2 rs
   return $ p2 ++ "_" ++ p1 ++ rs2
-}

rn = do
   p1 <- newSt pt1
   p2 <- newSt pt2
   rs <- get
   return $ p2 ++ "_" ++ p1 ++ rs
   where newSt re = StateT $ findFirstPrefix re

main :: IO ()
main = do
  args <- getArgs
  guard $ length args == 1
  let inputStr = args !! 0
  let s1 = maybe inputStr id (evalStateT rn inputStr)
  putStrLn s1
