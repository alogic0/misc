import Data.List (union, (\\),elemIndex)
import Data.Maybe (fromJust)

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

freeVars :: Expr -> Context
freeVars (Var s)   = [s] 
freeVars (f :@ a) = freeVars f `union` freeVars a 
freeVars (Lam i e) = freeVars e \\ [i]  

e2t :: Expr -> (Context,Term)
e2t exp = (fvs, removeNames fvs exp) where 
  fvs = freeVars exp
  removeNames ctx (Var s)    = Idx $ fromJust $ elemIndex s ctx
  removeNames ctx (e1 :@ e2) = removeNames ctx e1 :@: removeNames ctx e2
  removeNames ctx (Lam s e)  = Lmb s $ removeNames (s : ctx) e

freshVarName :: Symb -> [Symb] -> Symb
freshVarName j js 
  | j `elem` js = freshVarName (j ++ "'") js
  | otherwise   = j

t2e :: Context -> Term -> Expr
t2e ctx (Idx n)   = Var $ ctx !! n
t2e ctx (u :@: w) = t2e ctx u :@ t2e ctx w
t2e ctx (Lmb v t) = Lam v' $ t2e (v' : ctx) t
  where v' = freshVarName v ctx
