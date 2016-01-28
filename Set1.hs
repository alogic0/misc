{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude
import Crypto.Hash
import Data.String

fiveRands :: [Integer]
fiveRands = addNum (mkSeed 1) [1 .. 5]
  where
  addNum _ [] = []
  addNum s (_:ns) =
    let (n, s1) = rand s
    in n : addNum s1 ns
    
randLetter :: Seed -> (Char, Seed)
randLetter s =
  let (n, s1) = rand s
  in (toLetter n, s1)

randString3 :: String
randString3 = addChar (mkSeed 1) [1 .. 3]
  where
  addChar _ [] = []
  addChar s (_:xs) = 
    let (c, s1) = randLetter s
    in c : addChar s1 xs

test_rndStr3 =
  show (hash (fromString randString3) :: Digest SHA256) == 
  "9d475eb78d3e38085220ed6ebde9d8f7d26540bb1c8f9382479c3acd4c8c94a3"

type Gen a = Seed -> (a, Seed)

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo g f = \s ->
  let (m, s1) = g s
  in f m s1

mkGen :: a -> Gen a
mkGen x = \s -> (x, s)

randPair :: Gen (Char, Integer)
randPair = randLetter `genTwo` \c -> rand `genTwo` \n -> mkGen (c, n)

repRandom :: [Gen a] -> Gen [a]
repRandom [] = mkGen []
repRandom (x:xs) = x `genTwo` \n -> repRandom xs `genTwo` \ns -> mkGen (n:ns)

