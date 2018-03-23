type Symb = String 

infixl 2 :@:
infixl 2 :@

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq, Read, Show)

data Term = Idx Int
          | Term :@: Term
          | Lmb Symb Term
          deriving (Read, Show)

instance Eq Term where
  Idx m     == Idx n      =  m == n
  (t1:@:s1) == (t2:@:s2)  =  t1 == t2 && s1 == s2
  Lmb _ t1  == Lmb _ t2   =  t1 == t2
  _         == _          =  False

type Context = [Symb]


depth :: Expr -> Int
depth (Var _) = 0
depth (Lam _ e) = 1 + depth e
depth (e1 :@ e2) = max (depth e1) (depth e2)

e2tC :: Int -> Context -> Context -> Expr -> (Context, Term)
e2tC  bigN cont freeVars  (Var s) 
  | (Just n) <- lookup s (zip cont [0..]) = (freeVars, Idx n)
  | (Just n) <- lookup s (zip freeVars [0..]) = (freeVars, Idx (bigN + n))
  | otherwise = (freeVars ++ [s], Idx (bigN + length freeVars))
e2tC bigN cont freeVars (Lam s e)
  | (fv1, t1) <- e2tC bigN (s : cont) freeVars e = (fv1, Lmb s t1)
e2tC bigN cont freeVars (e1 :@ e2) =
  let (fv1, t1) = e2tC bigN cont freeVars e1
      (fv2, t2) = e2tC bigN cont fv1 e2
  in (fv2, t1 :@: t2)

e2t :: Expr -> (Context,Term)
e2t e = e2tC (depth e) [] [] e

{- Test e2t
one = Lam "s" $ Lam "z" $ Var "s" :@ Var "z"
e2t one == ([],Lmb "s" (Lmb "z" (Idx 1 :@: Idx 0)))
tst1 = Lam "x" $ Var "x" :@ Var "y" :@ Var "z"
e2t tst1 == (["y","z"],Lmb "x" ((Idx 0 :@: Idx 1) :@: Idx 2))
-}
