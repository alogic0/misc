module TreeByLevels where
import Data.List

data TreeNode a = TreeNode {
  left  :: Maybe (TreeNode a),
  right :: Maybe (TreeNode a),
  value :: a
  } deriving Show

buildTree :: [a] -> Maybe (TreeNode a)
buildTree []     = Nothing
buildTree (v:vs) = Just $ TreeNode (buildTree lvs) (buildTree rvs) v
  where (lvs, rvs) = splitAt ((length vs + 1) `div` 2) vs

mark :: String -> TreeNode a -> TreeNode (a, String)
mark str tree =
 tree { left = do t <- left tree
                  return $ mark (str ++ "L") t
       ,right = do t <- right tree
                   return $ mark (str ++ "R") t
       ,value = (value tree, str)
      }

flat :: TreeNode a -> [a]
flat tree = [value tree] ++ maybe [] flat (left tree) ++ maybe [] flat (right tree)

nodeSort = sortBy (\(_,s1) (_,s2) -> compare s1 s2)

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels Nothing = []
treeByLevels (Just tree) =
  fst $ unzip $ nodeSort $ flat $ mark "" tree
