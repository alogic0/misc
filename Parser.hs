module Parser where
import Control.Monad.State
import Data.Char

type Parser = StateT String []

runParser = runStateT

result :: a -> Parser a
result = return

{-zero :: Parser a
zero = StateT $ \inp -> []
-}
item :: Parser Char
item = do
        inp <- get
        case inp of
            (x:xs) -> do put xs
                         return x
            []     -> mzero

sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           guard $ p c
           return c

char :: Char -> Parser Char
char x = sat (==x)

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

{-plus :: Parser a -> Parser a -> Parser a
plus p q = StateT $ \s -> 
            (++) (runParser p s)
                 (runParser q s)
-}

first :: Parser a -> Parser a
first p = StateT $ \s ->
        case (runParser p s) of
        [] -> []
        (x:xs) -> [x]

(+++) :: Parser a -> Parser a -> Parser a
(+++) p q = first (p `mplus` q)

letter :: Parser Char
letter = lower +++ upper

alphanum :: Parser Char
alphanum = letter +++ digit

string :: String -> Parser String
string "" = return ""
string str@(x:xs) = do char x
                       string xs
                       return str

many :: Parser a -> Parser [a]
many p = do x <- p
            xs <- many p
            return (x:xs)
         +++ return []

word :: Parser String
word = many letter

many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             xs <- many p
             return (x:xs)

nat :: Parser Int
nat = do ds <- many1 digit
         return $ eval ds
         where
         eval ds = foldl op 0 ds
         n `op` c = 10*n + (ord c - ord '0')

int :: Parser Int
int = do f <- op
         n <- nat
         return $ f n
         where
         op = (char '-' >> return negate) +++ return id

sepby1 :: Parser a -> Parser b -> Parser [a]
sepby1 p sep = do x <- p
                  xs <- many (sep >> p)
                  return (x:xs)

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do open
                          x <- p
                          close
                          return x

spaces :: Parser () 
spaces = do many1 $ sat isSpace
            return ()

comment :: Parser ()
comment = do string "--"
             many $ sat (/='\n')
             return ()

lcomment ::Parser ()
lcomment = do let skips = sat (\c -> not (elem c "-}")) 
              string "{-"
              many (many1 skips
                +++ (many1 (char '-') >> many1 skips)
                +++ many1 (char '}'))
              many1 (char '-')
              char '}'
              return ()

junk :: Parser ()
junk = do many (spaces +++ comment)
          return ()

parse :: Parser a -> Parser a
parse p = do junk
             p

token :: Parser a -> Parser a
token p = do junk
             x <- p
             junk
             return x

integer :: Parser Int
integer = token int

ints :: Parser [Int]
ints = bracket (char '[')
	           (integer `sepby1` char ',')
	           (char ']')


