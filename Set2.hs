{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Nothing | Just a

instance (Show a) => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "Just " ++ show a

instance (Eq a) => Eq (Maybe a) where
  Nothing == Nothing = True
  Just x == Just y = x == y
  _ == _ = False 

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link ma f =
  case ma of
    Nothing -> Nothing
    Just x  -> f x

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay m ls =
  headMay ls `link` \(k, v) -> 
    if k == m 
    then (Just v) 
    else tailMay ls `link` \ls1 -> lookupMay m ls1   

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay m n =
  if n == 0
  then Nothing
  else Just (m / n)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = maximumMay' x xs
  where
  maximumMay' x [] = Just x
  maximumMay' x (y:ys) = maximumMay' (max x y) ys

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x:xs) = minimumMay' x xs
  where
  minimumMay' x [] = Just x
  minimumMay' x (y:ys) = minimumMay' (min x y) ys

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gd str =
  lookupMay str gd `link` \xs -> 
    maximumMay xs `link` \m ->
      headMay xs `link` \h ->
        divMay (fromIntegral m) (fromIntegral h)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries db s1 s2 =
  lookupMay s1 db `link` \n1 ->
    lookupMay s2 db `link` \n2 ->
      Just (n1 + n2)

--  Data.ByteString.Base16.decode (Data.String.fromString "794C696E6B203A3A202861202D3E2062202D3E206329202D3E204D617962652061202D3E204D617962652062202D3E204D617962652063")

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f ma mb =
  ma `link` \m ->
    mb `link` \n ->
      Just (f m n)

addSalaries2 db s1 s2 = yLink (+) (lookupMay s1 db) (lookupMay s2 db)

mkMaybe :: a -> Maybe a
mkMaybe = Just

tailProd :: Num a => [a] -> Maybe a
tailProd xs = tailMay xs `link` \ys -> mkMaybe (product ys)

tailSum :: Num a => [a] -> Maybe a
tailSum xs = yLink (\ys _ -> sum ys) (tailMay xs) (mkMaybe ())
