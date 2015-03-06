hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

produce :: (a -> a) -> a -> [a]
produce f n = n : produce f (f n)

hailseq :: Integer -> [Integer]
hailseq n = takeWhile (/=2) $ produce hailstone n

{-maxHailseq :: Integer -> [Integer]
maxHailseq n =
  foldr (\xs ms 
	   -> if (length xs > length ms)
	      then xs
	      else ms)
	[]
  $ map hailseq [1 .. n]
-}


maxHailseq :: Integer -> (Int, Int)
maxHailseq n =
  foldr (\x@(n,l) x1@(n1,l1) 
	   -> if l > l1
	      then x
	      else x1)
	(2,0)
  $ zip [1..] $ map (length . hailseq) [1 .. n]
