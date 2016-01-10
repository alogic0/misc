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

data Trip = Trip String String [String]
    deriving Show

--mediaRe :: RE Char Trip
mediaRe = 
  Trip <$> ("@Media:\t" *> name) <* delim 
       <*> ("audio" <|> "video") 
       <*> many (delim *> ("missing" <|> "unlinked"))


name :: RE Char String
name = many (psym (noneOf " ,"))

data Info =
    Skip
  | Audio FilePath
  | Video FilePath
    deriving (Eq, Show)

noneOf :: [Char] -> Char -> Bool
noneOf = flip notElem

oneOf :: [Char] -> Char -> Bool
oneOf = flip elem


delim = foldr1 (liftA2 (++)) [many $ sym ' ', ",", many $ sym ' ']

audio = Audio <$> name <* delim <* "audio" 
video = Video <$> name <* delim <* "video" 
skip = Skip <$ (audio <|> video) <* delim <* ("missing" <|> "unlinked")

mediaRe1 = audio <|> video <|> skip
{- --Tests--
"@Media:\thas-audio-but-missing, audio, missing" =~ mediaRe
Just Skip
"@Media:\thas-video,video" =~ mediaRe
Just (Video "has-video")
"@Media:\thas-audio,   audio" =~ mediaRe
Just (Audio "has-audio")
-}
