import Text.Regex.Applicative

import Data.Char hiding (Space)

import Data.Maybe

data Lexeme
    = Number Int
    | Op Char
    | Identifier String
    | LParen
    | RParen
    deriving Show

num :: RE Char Int
num = read <$> many (psym isDigit)

op :: RE Char Char
op = foldr1 (<|>) $ map sym ['+', '-', '/', '*']

identifier :: RE Char String
identifier = (:) <$> psym isAlpha <*> many (psym isAlphaNum)

space :: RE Char String
space = many $ psym isSpace

lexeme :: RE Char Lexeme
lexeme = (Number <$> num)
    <|> (Op <$> op)
    <|> (Identifier <$> identifier)
    <|> (LParen <$ sym '(')
    <|> (RParen <$ sym ')')

lexemes :: RE Char [Lexeme]
lexemes = catMaybes <$> many ((Just <$> lexeme) <|> (Nothing <$ space))

main = print $ "a + 2*b - 3/c" =~ lexemes
