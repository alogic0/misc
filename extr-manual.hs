module Main where

import Text.HTML.TagSoup

import System.Environment (getArgs, getProgName)

getReadme :: FilePath -> IO ()
getReadme fn = do
    src <- readFile fn
    let srcL = parseTags src
--    putStr $ show2 $ initL ++ extractReadme srcL ++ lastL
    putStr $ renderTags $ initL1 ++ (title srcL : initL2) ++ extractReadme srcL ++ lastL
    where
      title = (!! 1) . dropWhile (~/= TagOpen "title" [])
      extractReadme = takeWhile (~/= TagClose "div") . dropWhile (~/= TagOpen "div" [("class","embedded-author-content")])

main :: IO ()
main = do
  args <- getArgs
  case args of
    (fn:_) -> getReadme fn
    [] -> getProgName >>= \nm -> 
      do putStrLn "Missing filename"
         putStrLn $ "Usage: " ++ nm ++ " file > manual"

initL1 = [TagOpen "!DOCTYPE" [("html","")]
  ,TagText "\n"
  ,TagOpen "html" []
  ,TagText "\n"
  ,TagOpen "head" []
  ,TagText "\n  \n  "
  ,TagOpen "meta" [("name","viewport"),("content","width=device-width, initial-scale=1")]
  ,TagText "\n"
  ,TagOpen "link" [("rel","stylesheet"),("href","hackage.css"),("type","text/css")]
  ,TagClose "link"
  ,TagOpen "title" []]

initL2 = [TagClose "title"
  ,TagText "\n  "
  ,TagText "\n"
  ,TagClose "head"
  ,TagText "\n\n"
  ,TagOpen "body" []
  ,TagText "\n"
  ,TagOpen "div" [("class","embedded-author-content")]]

lastL = [TagClose "div"
  ,TagText "\n\n  \n"
  ,TagClose "body"
  ,TagText "\n"
  ,TagClose "html"]


show2 :: Show a => [Tag a] -> String
show2 [] = "[]"
show2 xs = "[" ++ concat (intersperseNotBroken "\n," $ map show xs) ++ "\n]\n"


-- the standard intersperse has a strictness bug which sucks!
intersperseNotBroken :: a -> [a] -> [a]
intersperseNotBroken _ [] = []
intersperseNotBroken sep (x:xs) = x : is xs
    where
        is [] = []
        is (y:ys) = sep : y : is ys
