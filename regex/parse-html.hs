{-# LANGUAGE OverloadedStrings #-}
-- module Main where

import Text.Regex.Applicative
import Control.Monad.State
import Data.Maybe (maybeToList)

collectM :: [StateT s Maybe a] -> s -> [a] 
collectM [] _ = []
collectM (l : ls) s = 
  case (runStateT l s) of
    (Just (a1, s1)) -> a1 : collectM ls s1
    Nothing -> []

aTag :: RE Char String
aTag = foldr1 (liftA2 (++)) ["<a href", many $ psym (/='<'), "</a>"]

takeInfix re = StateT $ \str -> (\(_, a, s) -> (a, s)) <$> findFirstInfix re str
