import Data.List (union, (\\),elemIndex)
import Data.Maybe (fromJust)
import Control.Applicative ((<|>))

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
freeVars (Var s) = [s]
freeVars (Lam s e) = (freeVars e) \\ [s]
freeVars (e1 :@ e2) = union (freeVars e1) (freeVars e2)

e2t :: Expr -> (Context,Term)
e2t e = (ctxE, e2tC ctxE e)
  where
  ctxE = freeVars e
  e2tC ctx (Var s) = Idx $ fromJust $ elemIndex s ctx
  e2tC ctx (Lam s e) = Lmb s $ e2tC (s : ctx) e
  e2tC ctx (e1 :@ e2) = (e2tC ctx e1) :@: (e2tC ctx e2)



{- Test e2t
one = Lam "s" $ Lam "z" $ Var "s" :@ Var "z"
e2t one == ([],Lmb "s" (Lmb "z" (Idx 1 :@: Idx 0)))
tst1 = Lam "x" $ Var "x" :@ Var "y" :@ Var "z"
e2t tst1 == (["y","z"],Lmb "x" ((Idx 0 :@: Idx 1) :@: Idx 2))
-}


t2e :: Context -> Term -> Expr
t2e ctx (Idx n) = Var (ctx !! n)
t2e ctx (Lmb s t)
  | s `elem` ctx = Lam s1 $ t2e (s1 : ctx) t
  | otherwise = Lam s $ t2e (s : ctx) t
  where 
    names = zipWith (:) (repeat 'x') (map show [0..])
    s1 = head (names \\ ctx)
t2e ctx (t1 :@: t2) = (t2e ctx t1) :@ (t2e ctx t2)

{- Test t2e
twoDB = Lmb "s" (Lmb "z" (Idx 1 :@: (Idx 1 :@: Idx 0)))
t2e [] twoDB == Lam "s" (Lam "z" (Var "s" :@ (Var "s" :@ Var "z")))
(snd . e2t . t2e []) twoDB == twoDB
tst1 = Lam "x" $ Var "x" :@ Var "y" :@ Var "z"
e2t tst1 == (["y","z"],Lmb "x" ((Idx 0 :@: Idx 1) :@: Idx 2))
uncurry t2e (e2t tst1) == Lam "x" ((Var "x" :@ Var "y") :@ Var "z")
uncurry t2e (e2t tst1) == tst1
-}



shift :: Int -> Term -> Term
shift val trm = shift' 0 val trm

shift' :: Int -> Int -> Term -> Term
shift' n val trm =
  case trm of
    (Idx id) -> Idx $
      if id < n
        then id
        else id + val
    (trm1 :@: trm2) -> (:@:) 
        (shift' n val trm1)
        (shift' n val trm2)
    (Lmb sym trm) -> Lmb sym $
      shift' (n + 1) val trm
    

substDB :: Int -> Term -> Term -> Term
substDB j s (Idx i)|i == j = s
substDB j s (Idx i)|otherwise = Idx i
substDB j s (t1 :@: t2) = (substDB j s t1) :@: (substDB j s t2)
substDB j s (Lmb sym t) = Lmb sym $ substDB (j + 1) (shift 1 s) t

betaRuleDB :: Term -> Term
betaRuleDB (Lmb _ t :@: s) = shift (-1) $ substDB 0 (shift 1 s) t
betaRuleDB t = t

oneStepDBN :: Term -> Maybe Term
oneStepDBN (Idx _) = Nothing
oneStepDBN t@(Lmb _ _ :@: _) = Just $ betaRuleDB t
oneStepDBN t@(t1 :@: t2) = (:@: t2) <$> oneStepDBN t1 <|> (t1 :@:) <$> oneStepDBN t2
oneStepDBN (Lmb sym t) = Lmb sym <$> oneStepDBN t


oneStepDBA :: Term -> Maybe Term
oneStepDBA (Idx _) = Nothing
oneStepDBA t@(Lmb sym t1 :@: t2)
  | (Just t2') <- oneStepDBA t2 = return (Lmb sym t1 :@: t2')
  | otherwise = Just $ betaRuleDB t
oneStepDBA (t1 :@: t2) = (:@: t2) <$> oneStepDBA t1 <|> (t1 :@:) <$> oneStepDBA t2
oneStepDBA (Lmb sym t) = Lmb sym <$> oneStepDBA t

nfDB :: (Term -> Maybe Term) -> Term -> Term 
nfDB f t = 
  case (f t) of
    (Just t1) -> nfDB f t1
    Nothing -> t


{- Test nfDB
nfDBN = nfDB oneStepDBN
cIDB = Lmb (Idx 0)
cKDB = Lmb (Lmb (Idx 1))
comegaDB = Lmb (Idx 0 :@: Idx 0)
cOmegaDB = comegaDB :@: comegaDB
nfDBN (cKDB :@: cIDB :@: cOmegaDB)
-}

{- Test oneStepDBx
cIDB = Lmb (Idx 0)
comegaDB = Lmb (Idx 0 :@: Idx 0)
cOmegaDB = comegaDB :@: comegaDB
test = cIDB :@: cOmegaDB
oneStepDBN test == Just cOmegaDB
oneStepDBA test == Just test
-}

{- Test betaRuleDB
betaRuleDB ((Lmb $ Lmb $ Idx 0) :@: Idx 41) == Lmb (Idx 0)
betaRuleDB ((Lmb $ Lmb $ Idx 1) :@: Idx 41) == Lmb (Idx 42)
-}

{- Test substDB
test = Lmb ((Idx 2 :@: Idx 0) :@: Idx 1)
substDB 0 (Idx 0) test == Lmb ((Idx 2 :@: Idx 0) :@: Idx 1)
substDB 0 (Idx 1) test == Lmb ((Idx 2 :@: Idx 0) :@: Idx 2)
substDB 0 (Idx 2) test == Lmb ((Idx 2 :@: Idx 0) :@: Idx 3)
substDB 1 (Idx 0) test == Lmb ((Idx 1 :@: Idx 0) :@: Idx 1)
substDB 2 (Idx 0) test == Lmb ((Idx 2 :@: Idx 0) :@: Idx 1)
-}

{- Test shift
test = Lmb (Lmb (Lmb ((Idx 2 :@: Idx 3) :@: Idx 1)))
shift 4 test == Lmb (Lmb (Lmb ((Idx 2 :@: Idx 7) :@: Idx 1)))
(shift (-2) . shift 2) test == test
-}

{-
hasRedex :: Term -> Bool
hasRedex (Lmb _ :@: _) = True
hasRedex (t1 :@: t2) = hasRedex t1 || hasRedex t2
hasRedex (Lmb t1) = hasRedex t1
hasRedex _ = False
-}

nfDBAe e =
  let (ctx, trm) = e2t e
  in t2e ctx $ nfDB oneStepDBA trm

e2t0 = snd . e2t

t2e0 = t2e []

{- Booleans
tru = Lam "x" (Lam "y" (Var "x"))
fls = Lam "x" (Lam "y" (Var "y"))
cond = Lam "i" (Lam "t" (Lam "f" ((Var "i" :@ Var "t") :@ Var "f")))
pair = Lam "f" (Lam "s" (Lam "b" ((Var "b" :@ Var "f") :@ Var "s")))
fstp = Lam "p" (Var "p" :@ tru)
sndp = Lam "p" (Var "p" :@ fls)
chNum n = Lam "s" $ Lam "z" $ foldr (:@) (Var "z") $ replicate n (Var "s")
scc = Lam "n" $ Lam "s" $ Lam "z" $ Var "s" :@ (Var "n" :@ Var "s" :@ Var "z")
plus' = Lam "f" (Lam "m" (Lam "n" (Var "m" :@ Var "f" :@ Var "n")))
plus = t2e0 $ nfDB oneStepDBA (e2t0 plus' :@: e2t0 scc)
times' = Lam "f" $ Lam "z" $ Lam "m" $ Lam "n" $ Var "n" :@ (Var "f" :@ Var "m") :@ Var "z"
times = t2e0 $ nfDB oneStepDBA (e2t0 times' :@: e2t0 plus :@: e2t0 (chNum 0))
power = t2e0 $ nfDB oneStepDBA (e2t0 times' :@: e2t0 times :@: e2t0 (chNum 1))
-}
