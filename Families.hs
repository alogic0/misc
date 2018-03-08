{---------
  Changing fmaily-names diversity over generations
  run in GHCi
  > let pX = initPairs
  > pX <- stepPairs pX

 --repeat last line to see changes in the families of the population
 ---------}

module Families where
import System.Random
import Control.Monad
import Data.List

numPop = 20
num = numPop `div` 2

type Generation = [Pair]
data Pair = Pair {husband::Person, wife::Person, idFamily::Int}
instance Show Pair where
    show (Pair h w iF) = "Pair\n  " 
                            ++ show h ++ "\n  "
                            ++ show w ++ "\n  "
                            ++ show iF
                            
type Child = Person
data Person = Person {familyP::String, nameP::Int, sexP::Gender, idParents::Int, idGen::Int}
    deriving (Show, Eq)
--type Family = String
data Gender = M | F 
            deriving (Eq, Ord, Enum, Bounded, Show)

instance Random Gender where
    randomR (a1, a2) g =
        let (n, g1) = randomR (fromEnum a1, fromEnum a2) g
        in (toEnum n, g1)

    random = randomR (minBound, maxBound)

neg :: Gender -> Gender
neg M = F
neg F = M
    
initPairs :: [Pair]   
initPairs =
    let names = map (:"") $ take num ['A'..]
        husbands = zipWith5 Person names (repeat 0)(repeat M) (repeat 0) (repeat 0)
        wifes = zipWith5 Person names (repeat 0) (repeat F) (repeat 0) (repeat 0)
    in zipWith3 Pair husbands wifes [0..]

ofSpringNums :: Int -> [Pair] -> IO [(Pair, Int)]
ofSpringNums 0 xs = return $ zip xs (repeat 0)
ofSpringNums _ [] = return []
ofSpringNums n [x] = return [(x,n)]
ofSpringNums n (x:xs) = 
    do 
       m <- getStdRandom (randomR (0,max 1 2*(n `div` (length xs + 1))))
       xs' <- ofSpringNums (n - m) xs
       return $ (x,m) : xs'
    
ofSprings :: (Pair, Int) -> IO (Pair, [Child])
ofSprings (p, 0) = return (p, [])
ofSprings (p, 1) =
    do
       sex <- getStdRandom random
       return (p, [Person {familyP = familyP (husband p)
                          , nameP = 0
                          , sexP = sex
                          , idParents = idFamily p
                          , idGen = idGen (husband p) + 1}])
ofSprings (p, n) =
    do
       sex <- getStdRandom random
       (_, chs@(ch1:_)) <- ofSprings (p, (n - 1))
       let ch2 = ch1 {nameP = nameP ch1 + 1, sexP = sex}
       return (p, ch2 : chs)
       
nextGen :: [Pair] -> IO [Child]
nextGen ps = 
    ofSpringNums numPop ps
    >>= \pss -> concat <$> (map snd <$> mapM ofSprings pss)
    
--- test: map (\p -> (familyP p, sexP p, idParents p)) <$> nextGen initPairs
--- test: nextGen initPairs >>= mapM_ print

pairsFor :: Person -> [Person] -> [Person]
pairsFor p = filter (\q -> idParents p /= idParents q && sexP p /= sexP q)
    
genToPairs :: [Person] -> IO [Pair]
genToPairs [] = return []
genToPairs ps =
  do
    n <- getStdRandom (randomR (0, length ps - 1))
    let p = ps !! n
    let ps' = delete p ps
    let bridies = pairsFor p ps'
    if null bridies
        then genToPairs ps'
        else 
          do m <- getStdRandom (randomR (0, length bridies - 1))
             let p1 = bridies !! m
             psN <- genToPairs (delete p1 ps')
             let [h, w] = if sexP p == M then [p, p1] else [p1, p]
             let prN = Pair {husband = h
                            , wife = w --{familyP = familyP h}
                            , idFamily = length psN}
             return $ prN : psN


newPairs :: IO [Pair] -> IO [Pair]
newPairs m = m >>= nextGen  >>= genToPairs

stepPairs :: [Pair] -> IO [Pair]
stepPairs pairsX = 
    do print pairsX
       genY <- nextGen pairsX 
       mapM_ print genY
       genToPairs genY

