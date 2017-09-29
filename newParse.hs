import Data.Char

newtype Parser a = Parser { apply :: String -> [(a, String)] }

-- (Тип apply это не что иное, как стандартный ReadS.) Однако, 
-- начав таким образом, лучше быть консистентным, поддерживая 
-- эту возможность повсюду, например

parse :: Parser a -> String -> [a]
parse p = map fst . filter (null . snd) . apply p

instance Functor Parser where
  fmap f p = Parser $ \str -> 
    [(f a, s1) | (a,s1) <- apply p str]

instance Applicative Parser where
--  empty = Parser $ \_ -> []
  pure c = Parser $ \str -> [(c,str)]

{-
char :: Char -> Parser Char
char c = fmap (\x -> if x == c 
                     then pure c
                     else empty)
-}
-- Сделайте приведенный выше парсер представителем Applicative 
-- и Alternative, так чтобы обеспечить следующее поведение
{-
> twoChars x = (\a b -> [a,b]) <$> char x <*> char x
> threeChars x = (\a b c -> [a,b,c]) <$> char x <*> char x <*> char x
> parse (some (twoChars '7') <|> some (threeChars '7')) "777777"
[["77","77","77"],["777","777"]]
-}
