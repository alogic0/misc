-- Ю.Г. Карпов "Конспект к курсу математической логики"
-- https://courses.openedu.ru/courses/course-v1:spbstu+MATLOG+fall_2015

import Data.List
import Data.Function
import Data.Maybe

bVal = [False, True]
imp s f = not s || f
xor x y = not (x == y)

(+!) :: Int -> Int -> Int
(+!) a b = (a + b) `mod` 2 

infixl 2 +!

{---- 2 ------
f1 = [not a || a | a <- bVal]
f2 = [(s `imp` f) && ( not s `imp` not f) | s <- bVal, f <- bVal]
f3 = [(s && h) `imp` f == (s `imp` f) || (h `imp` f) | s <- bVal, h <- bVal, f <- bVal]
f4 = [(b && d) `imp` f == (f || d) `imp` not f | b <- bVal, d <- bVal, f <- bVal]
-}

---- 3 -----
f1 a b c = b == a
f3 a b c = c `imp` a
f4 a b c = b || f3 a b c
f2 a b c = not (f1 a b c)
f5 a b c = f2 a b c `xor` f4 a b c 

f1Tab = [f1 a b c | a <- bVal, b <- bVal, c <- bVal]
f2Tab = [f2 a b c | a <- bVal, b <- bVal, c <- bVal]
f3Tab = [f3 a b c | a <- bVal, b <- bVal, c <- bVal]
f4Tab = [f4 a b c | a <- bVal, b <- bVal, c <- bVal]
f5Tab = [f5 a b c | a <- bVal, b <- bVal, c <- bVal]

ans = map (length . filter id) [f1Tab, f2Tab, f3Tab, f4Tab, f5Tab]

bools = [False, True]

abcd = map (fst . unzip . filter snd . zip "abcd") [[a,b,c,d]|a <- bools, b <- bools, c <- bools, d <- bools]

tbl :: [(String, Int)]
tbl = zip abcd [0,0,1,0,0,0,1,1,0,0,0,0,1,0,0,0]

zgl' = sortBy (compare `on` (length . fst)) tbl


zgl :: [(String, Int)]
{-
lkp' str = (fromJust . lookup str) zgl

zgl = [("",0),("d",0),("c",1),("b",0),("a",0)
      ,("cd",0 +! (lkp' "") +! (lkp' "c") +! (lkp' "d"))
      ,("bd",0 +! (lkp' "") +! (lkp' "b") +! (lkp' "d"))
      ,("bc",1 +! (lkp' "") +! (lkp' "b") +! (lkp' "c"))
      ,("ad",0 +! (lkp' "") +! (lkp' "a") +! (lkp' "d"))
      ,("ac",0 +! (lkp' "") +! (lkp' "a") +! (lkp' "c"))
      ,("ab",1 +! (lkp' "") +! (lkp' "a") +! (lkp' "b"))
      ,("bcd",1 +! (lkp' "") +! (lkp' "b") +! (lkp' "c") +! (lkp' "d") +! (lkp' "bc") +! (lkp' "bd") +! (lkp' "cd"))
      ,("acd",0 +! (lkp' "") +! (lkp' "a") +! (lkp' "c") +! (lkp' "d") +! (lkp' "ac") +! (lkp' "ad") +! (lkp' "cd"))
      ,("abd",0 +! (lkp' "") +! (lkp' "a") +! (lkp' "b") +! (lkp' "d") +! (lkp' "ab") +! (lkp' "ad") +! (lkp' "bd"))
      ,("abc",0 +! (lkp' "") +! (lkp' "a") +! (lkp' "b") +! (lkp' "c") +! (lkp' "ab") +! (lkp' "ac") +! (lkp' "bc"))
      ,("abcd",0 +! (lkp' "") +! (lkp' "a") +! (lkp' "b") +! (lkp' "c") +! (lkp' "d")
                 +! (lkp' "ab") +! (lkp' "ac") +! (lkp' "ad")
                 +! (lkp' "bc") +! (lkp' "bd") +! (lkp' "cd"))]
-}
-- mapM_ (putStrLn . show) zgl
                   
lkp str lst = fromJust $ lookup str lst
zgl = foldr expandList [head zgl'] (reverse $ tail zgl')
  where
  expandList (str,n) lst = (str, calcSubsec str n lst) : lst
  calcSubsec str n lst = foldr (+!) n (map (\s -> lkp s lst) (init $ subsequences str))
  
zglTxt = concat $ intersperse " +! " $ map (\(s,n) -> show n ++ "*" ++ intersperse '*' s) zgl
                 
zglP a b c d = 0 +! 0*d +! 1*c +! 0*b +! 0*a +! 1*c*d +! 0*b*d +! 0*b*c +! 0*a*d +! 1*a*c +! 1*a*b +! 1*b*c*d 
                 +! 1*a*c*d +! 1*a*b*d +! 1*a*b*c +! 0*a*b*c*d
           
zglPTbl = [((a,b,c,d),zglP a b c d) | let bools = [0, 1], a <- bools, b <- bools, c <- bools, d <- bools]           
           
tstZgl = [zglP a b c d | let bools = [0, 1], a <- bools, b <- bools, c <- bools, d <- bools]
         == [0,0,1,0,0,0,1,1,0,0,0,0,1,0,0,0]

boolTable :: String -> [[(String, Bool)]]
boolTable [] = [[]]         
boolTable (c:cs) = 
  do a <- bools
     map (([c], a) :) $ boolTable cs
       