import Control.Applicative

infixl 2 :@:

data Term = Idx Int
          | Term :@: Term
          | Lmb Term
          deriving (Eq, Read, Show)

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
    (Lmb trm) -> Lmb $
      shift' (n + 1) val trm
    

substDB :: Int -> Term -> Term -> Term
substDB j s (Idx i)|i == j = s
substDB j s (Idx i)|otherwise = Idx i
substDB j s (t1 :@: t2) = (substDB j s t1) :@: (substDB j s t2)
substDB j s (Lmb t) = Lmb $ substDB (j + 1) (shift 1 s) t

betaRuleDB :: Term -> Term
betaRuleDB (Lmb t :@: s) = shift (-1) $ substDB 0 (shift 1 s) t
betaRuleDB t = t

oneStepDBN :: Term -> Maybe Term
oneStepDBN (Idx _) = Nothing
oneStepDBN t@(Lmb _ :@: _) = Just $ betaRuleDB t
oneStepDBN t@(t1 :@: t2) = (:@: t2) <$> oneStepDBN t1 <|> (t1 :@:) <$> oneStepDBN t2
oneStepDBN (Lmb t) = Lmb <$> oneStepDBN t


oneStepDBA :: Term -> Maybe Term
oneStepDBA (Idx _) = Nothing
oneStepDBA t@(Lmb t1 :@: t2)
  | (Just t2') <- oneStepDBA t2 = return (Lmb t1 :@: t2')
  | otherwise = Just $ betaRuleDB t
oneStepDBA (t1 :@: t2) = (:@: t2) <$> oneStepDBA t1 <|> (t1 :@:) <$> oneStepDBA t2
oneStepDBA (Lmb t) = Lmb <$> oneStepDBA t

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
