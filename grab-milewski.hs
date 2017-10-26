module Main where

import Data.Char
import System.Environment (getArgs)
import System.IO
  ( IOMode(ReadMode)
  , hGetContents
  , hSetEncoding
  , stdout
  , utf8
  , withFile
  )
import Text.HTML.TagSoup

extractBody :: String -> [Tag String]
extractBody str =
  let getBody =
        takeWhile (~/= TagText "Follow @BartoszMilewski") .
        tail . dropWhile (~/= TagClose "head")
  in getBody $ parseTags str

main :: IO ()
main = do
  args <- getArgs
  hSetEncoding stdout utf8
  withFile
    (args !! 0)
    ReadMode
    (\handle -> do
       hSetEncoding handle utf8
       content <- hGetContents handle
       putStr $ renderTags (header ++ extractBody content ++ footer))

header :: [Tag String]
header =
  [ TagOpen
      "!DOCTYPE"
      [ ("html", "")
      , ("PUBLIC", "")
      , ("", "-//W3C//DTD XHTML 1.0 Transitional//EN")
      , ("", "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd")
      ]
  , TagText "\n"
  , TagOpen "html" [("xmlns", "http://www.w3.org/1999/xhtml"), ("lang", "en")]
  , TagText "\n"
  , TagOpen "head" [("profile", "http://gmpg.org/xfn/11")]
  , TagText "\n\t"
  , TagOpen
      "meta"
      [("http-equiv", "Content-Type"), ("content", "text/html; charset=UTF-8")]
  , TagClose "meta"
  , TagText "\n\t"
  , TagOpen "title" []
  , TagText
      "Category Theory for Programmers: The Preface | Bartosz Milewski's Programming Cafe"
  , TagClose "title"
  , TagText "\n\t"
  , TagClose "head"
  , TagText "\n\n"
  ]

footer :: [Tag String]
footer = [TagClose "body", TagText "\n", TagClose "html", TagText "\n"]
