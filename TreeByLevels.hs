module TreeByLevels where

data TreeNode a = TreeNode {
  left  :: Maybe (TreeNode a),
  right :: Maybe (TreeNode a),
  value :: a
  } deriving Show
  
mark str tree =
 tree { left = do t <- left tree
                  return $ mark (str ++ "L") t
       ,rigth = do t <- right tree
                   return $ mark (str ++ "R") t
       ,value = (value tree, str)
      }

treeByLevels :: Maybe (TreeNode a) -> [a]
