import Test.QuickCheck

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

----------------------1
qsort1 [] = []
qsort1 (x:xs) = qsort1 larger ++ [x] ++ qsort1 smaller
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

----------------------2
qsort2 [] = []
qsort2 (x:xs) = reverse (qsort2 larger ++ [x] ++ qsort2 smaller)
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

----------------------3
qsort3 [] = []
qsort3 (xs) = qsort3 larger ++ qsort3 smaller ++ [x]
  where x = minimum xs
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

----------------------4
qsort4 [] = []
qsort4 (x:xs) = reverse (qsort4 smaller) ++ [x] ++ reverse (qsort4 larger)
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]


prop::[Int] -> Bool
prop = \xs -> qsort3 xs == reverse (qsort xs)
