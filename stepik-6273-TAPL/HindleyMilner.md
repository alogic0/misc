To beautify types and write them like in usual Haskell, the next mess of code is produced. To work it requires
```bash
cabal install regex-applicative megaparsec 
```
Place this file in the same directory as `HindleyMilner.hs`, run **ghci** there and load the code
```bash
>:l TypeParser.hs
```
then copy and paste the strings with `test` commands from the end of file. Now you can `test` various simple types for possible unification. Look at the resulting type and the required substitutions for the originals to mutate into it. The order of substitutions from left to right.

##  TypeParser.hs
```haskell
module TypeParser where

import Control.Monad (void, guard)
import Data.Void
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Char 
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.State as S
import qualified Text.Regex.Applicative as RE
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

re1 = RE.string " " *> RE.string ")"
re2 = RE.string "(" <* RE.string " " 

prettyShow :: Type -> String
prettyShow = RE.replace re2 . RE.replace re1 . unwords 
             . map (filter (flip notElem ":\""))
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


test a b = do
  print $ testS a b
  print $ unifyS a b 

{- Tests
test "b -> a -> b "      "(g -> g) -> d"
test "a -> ( b -> c )"   "d -> d"
test "b -> b"              "((g -> d) -> e) -> (a -> d)"
test "a -> b -> a"       "g -> d"
-}
```
