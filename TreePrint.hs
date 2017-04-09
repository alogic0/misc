-- Pretty printer for Tree
-- was a task on
-- https://stepik.org/lesson/8535/
-- 
-- example: print testTree

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq)

render :: (Show a) => Tree a -> [String]
--rnder (Leaf x) = [show x]
render Nil = [" "]
render (Branch l x r) =
  let l' = render l
      r' = render r
      wL = widthT l'
      wR = widthT r'
      wX = length $ show x
      pref = replicate wX ' '
      line1 = replicate wL ' ' ++ show x
              ++ replicate wR ' '
  in
      line1 : sumR l' (map (pref ++) r')

widthT = length . head
heightT = length

sumR l r
  | null l = r
  | null r = l
  | otherwise =
      let wL = widthT l
          wR = widthT r
          hL = heightT l
          hR = heightT r
          n = hL - hR
          n' = abs n
      in
          case  compare n 0 of
            GT -> sumR (take hR l) r
                  ++ sumR (drop hR l) (replicate n' (replicate wR ' '))
            LT -> sumR l (take hL r)
                  ++ sumR (replicate n' (replicate wL ' ')) (drop hL r)
            EQ -> zipWith (++) l r

instance Show a => Show (Tree a) where
   show Nil = "\n"
   show t = unlines $ render t

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
