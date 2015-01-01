import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad

ws :: Parser String
ws = many space

lexeme p = p <* ws

data JSONValue = 
    B Bool
  | S String
  | A [JSONValue]
  | O [(String, JSONValue)]
 deriving Show

stringLiteral :: Parser String
stringLiteral = char '"' *> many (noneOf ['"']) <* char '"'

bool :: Parser Bool
bool = (string "true" *> pure True )
    <|>(string "false" *> pure False) 

array :: Parser [JSONValue]
array = 
  (lexeme $ char '[')
  *>
  ((lexeme jsonValue) `sepBy` (lexeme $ char ','))
  <*
  (lexeme $ char ']')

jsonObject :: Parser [(String, JSONValue)]
jsonObject =
  (lexeme $ char '{')
  *>
  ((lexeme objectEntry) `sepBy` (lexeme $ char ','))
  <*
  (lexeme $ char '}')

objectEntry :: Parser (String, JSONValue)
objectEntry = 
  do key <- lexeme stringLiteral
     lexeme $ char ':'
     value <- lexeme jsonValue
     return (key, value)

jsonValue :: Parser JSONValue
jsonValue = (B <$> bool)
	<|> (S <$> stringLiteral)
	<|> (A <$> array)
	<|> (O <$> jsonObject)

(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = (try p) <|> q

day :: Parser Int
day =  (string "Monday" *> pure 1)
  <||> (string "Tuesday" *> pure 2)
  <||> (string "Wednesday" *> pure 3)
  <||> (string "Thursday" *> pure 4)
  <||> (string "Friday" *> pure 5)
  <||> (string "Saturday" *> pure 5)
  <||> (string "Sunday" *> pure 5)
