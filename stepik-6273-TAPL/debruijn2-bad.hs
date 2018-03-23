import Control.Applicative

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

namesGen = zipWith (:) (repeat 'x') (map show [0..])

t2e :: Context -> Term -> Expr
t2e ctx t = 
  let (_, _, _, e) = t2eC (filter (\x -> not (elem x ctx)) namesGen) [] [] ctx t
  in e

t2eC
  :: [Symb]
     -> [Symb]
     -> [(Int, Symb)]
     -> [Symb]
     -> Term
     -> ([Symb],[(Int, Symb)], [Symb], Expr)
t2eC names boundVars freeVarsMap ctx (Idx n) 
  | n < length boundVars = (names, freeVarsMap, ctx, Var (boundVars !! n))
  | (Just sym) <- lookup n freeVarsMap = (names, freeVarsMap, ctx, Var sym)
  | otherwise = 
      let sym = head ctx
      in (names, freeVarsMap ++ [(n,sym)], tail ctx, Var sym)
t2eC names boundVars freeVarsMap ctx (Lmb sym trm)
  | elem sym boundVars = 
      let nm = head names
          (nms1, fvm1, ctx1, e1) = t2eC (tail names) (nm : boundVars) freeVarsMap ctx trm
      in (nms1, fvm1, ctx1, Lam nm e1)
  | otherwise =
      let nm = sym
          (nms1, fvm1, ctx1, e1) = t2eC names (nm : boundVars) freeVarsMap ctx trm
      in (nms1, fvm1, ctx1, Lam nm e1)
t2eC names boundVars freeVarsMap ctx (trm1 :@: trm2) =
  let (nms1, fvm1, ctx1, e1) = t2eC names boundVars freeVarsMap ctx trm1
      (nms2, fvm2, ctx2, e2) = t2eC nms1 boundVars fvm1 ctx1 trm2
  in (nms2, fvm2, ctx2, e1 :@ e2)

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

