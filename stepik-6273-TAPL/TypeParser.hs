module TypeParser where

import Control.Monad (void, guard)
import Data.Void
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Char 
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.State as S
import HindleyMilner

type Parser = Parsec Void String

symbol :: String -> Parser String
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

expr = makeExprParser term table <?> "expression"

term = parens expr <|> lexeme var <?> "term"

parens = between (symbol "(") (symbol ")")

var :: Parser Type
var = do
  name <- (:) <$> letterChar <*> many alphaNumChar
  return $ TVar name

table = [ [ binary  "->"  (:->) ] ]

binary  name f = InfixL  (f <$ symbol name)


seqSubst lst e = foldl (\e s -> fromJust $ subst [s] e) e lst

prettyShow :: Type -> String
prettyShow = unwords . map (filter (flip notElem ":\""))
             . filter (/="TVar") . S.evalState prs . show

testS aS bS = do
  a <- runParser expr "" aS
  b <- runParser expr "" bS
--  maybe (Left Nothing) Right $ do
  return $ do
    lst <- unify a b 
    let aNew = seqSubst lst a
    guard $ aNew == seqSubst lst b
    return $ prettyShow aNew
--    return aNew
    

unifyS aS bS = do
  a <- runParser expr "" aS
  b <- runParser expr "" bS
  return $ map (\(a,b) -> (prettyShow a, prettyShow b)) <$> unify a b 
--  return $ unify a b 

lexing :: S.State String String
lexing = S.state (head . lex)

prs = do 
  l <- lexing
  if l == ""
  then return []
  else do 
    ls <- prs
    return (l:ls)
