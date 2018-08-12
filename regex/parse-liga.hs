{-# LANGUAGE OverloadedStrings #-}
-- module Main where

import Text.Regex.Applicative
import Control.Monad.State
import Data.Maybe 
import System.Environment
import System.IO
import Data.Char (isDigit)

infixr 5 +++
(+++) :: RE Char String -> RE Char String -> RE Char String
(+++) = liftA2 (++)

collectM :: [StateT s Maybe a] -> s -> [a] 
collectM [] _ = []
collectM (l : ls) s = 
  case (runStateT l s) of
    (Just (a1, s1)) -> a1 : collectM ls s1
    Nothing -> []

aTag :: RE Char String
aTag = foldr1 (+++) ["<a href", many $ psym (/='<'), "</a>"]

hrefAddr = "<a href=" *> many (sym '"') *> many (psym (/='"')) <* many (sym '"') <* ">"

keyStr = foldr1 (+++) ["?key=", keyNum, "&version=", verNum]
keyNum = many (sym '-') +++ many (psym isDigit) 
verNum = many (psym isDigit <|> sym '.')


-- | Example: collectM (repeat $ takeInfix aTag) str
takeInfix re = StateT $ \str -> (\(_, a, s) -> (a, s)) <$> findFirstInfix re str

takeShortestInfix re = StateT $ \str -> (\(_, a, s) -> (a, s)) <$> findShortestInfix re str

getATags = collectM (repeat $ takeInfix aTag)

-- wget --user=${Login} --password=${Password} -q -O psw/${Login}-${Password}.html ${SITE}/download
-- iconv -f KOI8-R -t UTF-8 < psw/${Login}-${Password}.html >psw/tmp

tmpMain fp = withFile fp ReadMode $ \h -> do
    hSetEncoding h utf8 
    hSetEncoding stdout utf8
    str <- hGetContents h
    let tags1_all = getATags str
    let tags2_href = map fst $
          mapMaybe (findShortestPrefix hrefAddr) tags1_all
    let tags3_prm = mapMaybe (match keyStr) tags2_href

--  wget --user=${Login} --password=${Password} -q -O tmp/${Login}.html ${SITE}/download${PRM}
--  iconv -f KOI8-R -t UTF-8 < tmp/${Login}.html >tmp/tmp

    let links1 = map (\(a, b, c) -> a ++ b ++ c) $
          mapMaybe (findFirstInfix ("dvd" +++ ((:) <$> psym isDigit <*> "of") +++ ((:) <$> psym isDigit <*> ".iso"))) tags2_href
--    mapM_ (hPutStrLn stdout) tags1_all
--    mapM_ (hPutStrLn stdout) tags2_href
--    mapM_ (hPutStrLn stdout) tags3_prm
    mapM_ (hPutStrLn stdout) links1

