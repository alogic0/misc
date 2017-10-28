module Main where

import Data.Tree
import Data.Tree.View
import System.Environment
import System.IO
import Text.HTML.TagSoup.Tree

main :: IO ()
main = do
  args <- getArgs
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  let infile = args !! 0
  withFile
    infile
    ReadMode
    (\handle -> do
       hSetEncoding handle utf8
       content <- hGetContents handle
       let outfile =
             (reverse . drop 1 . dropWhile (/= '.') . reverse) infile ++
             "-tree.html"
       hPutStrLn stderr outfile
       writeHtmlTree Nothing outfile (convTagTree $ parseTree content))

convTagTree :: [TagTree String] -> Tree NodeInfo
convTagTree xs =
  Node
  { rootLabel =
      NodeInfo
      {nodeBehavior = InitiallyExpanded, nodeName = "Node", nodeInfo = ""}
  , subForest = map convTagTree' xs
  }

convTagTree' :: TagTree String -> Tree NodeInfo
convTagTree' (TagLeaf x) =
  Node
  { rootLabel =
      NodeInfo
      { nodeBehavior = InitiallyCollapsed
      , nodeName = unwords $ take 4 $ words $ show x
      , nodeInfo = ""
      }
  , subForest = []
  }
convTagTree' (TagBranch nm attr xs') =
  Node
  { rootLabel =
      NodeInfo
      { nodeBehavior = InitiallyCollapsed
      , nodeName = unwords $ (nm :) $ take 5 $ words $ show attr
      , nodeInfo = ""
      }
  , subForest = map convTagTree' xs'
  }
