{-# LANGUAGE OverloadedStrings#-}

--http://conscientiousprogrammer.com/blog/2015/12/02/24-days-of-hackage-2015-day-2-regexes-with-pcre-heavy-standalone-haskell-scripts-using-stack/

module Main where

import Text.Regex.Applicative
import Data.Maybe (listToMaybe)
import Text.Printf (printf)

-- | Match a media name, audio/video, and optional missing/unlinked.
{-
mediaRegex :: Regex
mediaRegex = [re|^@Media:\t([^ ,]+)\ *,\ *(audio|video)(\ *,\ *(?:missing|unlinked))?|]
-}

data Info =
    Skip
  | Audio FilePath
  | Video FilePath
    deriving (Eq, Show)

fNotElem :: [Char] -> Char -> Bool
fNotElem = flip notElem

fElem :: [Char] -> Char -> Bool
fElem = flip elem

--name :: RE Char String
name = "@Media:\t" *> many (psym (fNotElem " ,"))

delim = foldr1 (liftA2 (++)) [many $ sym ' ', ",", many $ sym ' ']

audio = Audio <$> name <* delim <* "audio" 
video = Video <$> name <* delim <* "video" 
skip = Skip <$ (audio <|> video) <* delim <* ("missing" <|> "unlinked")

mediaRe = audio <|> video <|> skip

{- --Tests--
"@Media:\thas-audio-but-missing, audio, missing" =~ mediaRe
Just Skip
"@Media:\thas-video,video" =~ mediaRe
Just (Video "has-video")
"@Media:\thas-audio,   audio" =~ mediaRe
Just (Audio "has-audio")
-}
