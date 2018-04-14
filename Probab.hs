module Probab where

import Data.List
import Data.Ratio

fact n = product [1 .. n]

comb n m = ordComb n m `div` fact m

ordComb n m 
  | n >= m = product [(n - m + 1) .. n]
  | otherwise = error "Second arg is bigger"

--- Polinomials
----
--- (x**2 + 3*x + 2) {- is represented by: -} [2,3,1]
--- (20) {- is represented by: -} [20]
--- (x) {- is represented by: -} [0,1]
--- (c0 + c1*x + c2*x**2 + ...) {- is represented by: -} [c0,c1,c2,..]

-- show This gets rid of all the zeroes at the end of the representation.
toProper [] = []
toProper p = if (last p /= 0) -- this means there is nothing to do.
             then p 
             else toProper $ init p -- this means the last zero can be discarded.


addPoly p1 p2 = 
  if (length p1 >= length p2)
  then zipWith (+) p1 (p2 ++ repeat 0)
  else addPoly p2 p1

multiplyBy a p1 = map (a*) p1

multiplyByX p = 0:p

multPoly [] p2 = []
multPoly (p:p1) p2 = let pTimesP2 = multiplyBy p p2
                         xTimesP1Timesp2 = multiplyByX $ multPoly p1 p2
                     in addPoly pTimesP2 xTimesP1Timesp2

negatePoly = map negate

newtype Poly a = P [a] 

properP :: (Num a, Eq a) => [a] -> Poly a
properP = P . toProper

instance (Num a, Eq a) => Eq (Poly a) where
    (P a) == (P b) = toProper a == toProper b
instance (Num a, Eq a) => Num (Poly a) where
    (P a) + (P b) = properP $ addPoly a b
    (P a) * (P b) = properP $ multPoly a b
    negate (P a) = properP $ map negate a
    abs = undefined
    signum = undefined
    fromInteger i = properP [fromIntegral i]
showPoly [] = show 0
showPoly p =  let cOs = zip p [0..]
                  nonZeroCOs = filter (\(c,_) -> c /= 0) cOs
                  cShow c = if c == 1 
                            then "" 
                            else show c
                  nShow n = case n of 
                              0 -> ""
                              1 -> "x" 
                              m -> "x^" ++ show m
                  cnShow c n = if c == 1 && n == 0 
                               then show 1 
                               else intercalate " " $ filter (/="") [cShow c, nShow n]            
                  terms = map (\(c,n) -> cnShow c n) nonZeroCOs
              in intercalate " + " (reverse terms)    

instance (Show a, Eq a, Num a) => Show (Poly a) where
    show (P a) = showPoly $ toProper a

toList (P p) = p

listOfPowers x = map (\n -> x ^ n) [0..]

makeFunction (P p) = \x -> sum $ zipWith (*) p (listOfPowers x)


--- taken N cards have at least 1 Ace. What chance to open Ace in that N?

mA = P [-4, 1]
gA = P [49, -1]

pA1 = (gA + 1) * (gA + 2) * (gA + 3)
pA2 = P [comb 3 1] * (mA + 3) * (gA + 2) * (gA + 3)
pA3 = P [comb 3 2] * (mA + 2) * (mA + 3) * (gA + 3)
pA4 = P [comb 3 3] * (mA + 1) * (mA + 2) * (mA + 3)

pNA = sum $ zipWith (*) [pA1, pA2, pA3, pA4] [P[1], P[2], P[3], P[4]]
pDA = sum [pA1, pA2, pA3, pA4] * P [0,1]
probN n = (makeFunction pNA n) % (makeFunction pDA n)
-- probN = pNA / pDA
-- probN n = (7350 * n + 117600)/(124950 * n)

-- pA k = length [(x, y) | x <- [1 .. 6], y <- [1 .. 6], x + y == k] % 36
-- pB = 1 % 6
-- [(k, n) | k <- [2 .. 12], n <- [1 .. 6], pAB k n == pA k * pB]

pBr n p k = fromIntegral (comb n k) * p ^ k * (1 - p) ^ (n - k)

disS = reverse $ take 11 $ [30 % 100, 40 % 100, 20 % 100] ++ repeat (5 % 100)
shoots = [[x1, x2, x3, x4, x5] | x1 <- [0 .. 10], x2 <- [0 .. 10], x3 <- [0 .. 10], x4 <- [0 .. 10], x5 <- [0 .. 10], x1 + x2 + x3 + x4 + x5 == 47]
pSidorR = fromRational $ sum [product pSh |sh <- shoots, let pSh = map (disS !!) sh]


shootsB cmp n = [[x1, x2, x3, x4, x5] | x1 <- [0 .. 10], x2 <- [0 .. x1], x3 <- [0 .. x2], x4 <- [0 .. x3], x5 <- [0 .. x4], cmp (x1 + x2 + x3 + x4 + x5) n]
pSidorRB cmp n = fromRational $ sum [k * product pSh |sh <- shootsB cmp n, let pSh = map (disS !!) sh, let k = (toInteger $ fact (length sh)) % (toInteger $ product $ map (fact . length) (group sh))]
