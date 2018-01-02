{-# LANGUAGE OverloadedStrings #-}

import Text.Regex.Applicative
import Data.Char (isAlphaNum)
import Data.List (sort, nub)
import Data.Maybe (maybe)

bElem :: RE Char String
bElem = "<" *> many (psym isAlphaNum) <* ">"

nonGtLt :: RE Char Char
nonGtLt = psym (`notElem` ("<>"::String))

collectElems :: RE Char [String]
collectElems = reFoldl NonGreedy (flip (:)) [] ((many nonGtLt) *> bElem <* (many nonGtLt))

main :: IO ()
main = do
  body <- readFile "block-elems.txt"
  putStrLn $ show $ maybe [] (nub . sort) $ match collectElems body
