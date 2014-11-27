{-# LANGUAGE NPlusKPatterns #-}
import Data.List
import Data.Char
import Unsafe.Coerce
import Test.QuickCheck
import Control.Monad

data Nat = Zero
         | Succ Nat
         deriving (Show, Eq)

integerToNat :: Integer -> Nat
integerToNat 0 = Zero
integerToNat n = Succ $ integerToNat (n-1)

prop_nat2int f = forAll (choose (0, 1000)) $ \n -> f (integerToNat n) == n
       
natToInteger1 Zero = 0
natToInteger1 (Succ n) = natToInteger1 n + 1

natToInteger2 (Succ n) = natToInteger2 n + 1
natToInteger2 Zero = 0

natToInteger4 (Succ n) = 1 + natToInteger4 n
natToInteger4 Zero = 0

natToInteger5 Zero = 1
natToInteger5 (Succ n) = (1 + natToInteger5 n) - 1

natToInteger6 = head . m
    where m Zero = [0]
          m (Succ n) = [sum [x | x <- (1 : m n)]]
          
natToInteger7 :: Nat -> Integer
natToInteger7 = \n -> genericLength [c | c <- show n, c == 'S']

--natToInteger8 :: Nat -> Integer
--natToInteger8 = \n -> length [c | c <- show n, c == 'S']

------------------------------- ex 1 ------------------------------------------

prop_int2nat f = forAll (choose (0, 1000)) $ \n -> f n == integerToNat n

integerToNat1 :: Integer -> Nat
integerToNat1 0 = Zero
integerToNat1 (n+1) = Succ (integerToNat1 n)

--integerToNat3 :: Integer -> Nat
--integerToNat3 n
--    = product [(unsafeCoerce c) :: Integer | c <- show n]

integerToNat5 :: Integer -> Nat
integerToNat5 (n+1) = Succ (integerToNat5 n)
integerToNat5 0 = Zero

integerToNat6 :: Integer -> Nat
integerToNat6 (n+1) = let m = integerToNat6 n in Succ m
integerToNat6 0 = Zero

--integerToNat8 :: Integer -> Nat
--integerToNat8 = \n -> genericLength [c | c <- show n, isDigit c]

------------------------------- ex 2 ------------------------------------

prop_add f = forAll (choose (0, 1000)) 
             $ \n -> forAll (choose (0, 1000))
                     $ \m -> let m' = integerToNat m
                                 n' = integerToNat n
                             in natToInteger1 (f m' n') == natToInteger1 m' + natToInteger1 n'
                             
add1 Zero n = n
add1 (Succ m) n = Succ (add1 n m)

add2 (Succ m) n = Succ (add2 n m)
add2 Zero n = n

add4 (Succ m) n = Succ (add4 m n)
add4 Zero n = Zero

add7 n Zero = n
add7 n (Succ m) = Succ (add7 m n)

add8 n (Succ m) = Succ (add8 m n)
add8 n Zero = n

---------------------------- ex 3 -----------------------------------------
add = add1
prop_mult f = forAll (choose (0, 100)) 
             $ \n -> forAll (choose (0, 100))
                     $ \m -> let m' = integerToNat m
                                 n' = integerToNat n
                             in natToInteger1 (f m' n') == natToInteger1 m' * natToInteger1 n'
                             
mult2 m Zero = Zero
mult2 m (Succ n) = add m (mult2 m n)

--------------------------- ex 4 ------------------------------------------
data Tree = Leaf Integer
          | Node Tree Integer Tree
          deriving Show

-- tree = oneof [liftM Leaf arbitrary,
	      -- liftM3 Node tree arbitrary tree]
tree = sized tree'
tree' 0 = liftM Leaf arbitrary
tree' n | n>0 = 
	oneof [liftM Leaf arbitrary,
	       liftM3 Node subtree arbitrary subtree]
  --where subtree = tree' (n `div` 2)
    where subtree = choose (0, (n - 1)) >>= \m -> tree' m

tree1 = Node (Node (Leaf 2) 8 (Leaf 4)) 8 (Node (Node (Node (Leaf (-4)) (-7) (Node (Node
    (Leaf 1) (-8) (Leaf (-8))) (-4) (Node (Leaf (-3)) 0 (Leaf (-8))))) (-3) (Node (
    Leaf (-5)) (-6) (Node (Leaf 5) 7 (Leaf (-3))))) (-7) (Leaf (-6)))
    
flatten :: Tree -> [Integer]
flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l
 ++ [n]
 ++ flatten r
    
binList k = [k, k - 1 .. 1] ++ [k + 1 .. 2 * k]

listToSTree [a] = Leaf a
listToSTree [] = error "Zero list"
listToSTree [a, b] = error "Only two elements"
listToSTree xs =
    let half = length xs `div` 2
        (xs1, xs2) = splitAt half xs
    in Node (listToSTree xs1)
            (head xs2)
            (listToSTree (tail xs2))

magNums = 1:(map (\k -> 2*k+1) magNums)           

