import Control.Monad

type Symb = String
infixl 2 :@: 
infixr 3 :->
infixl 4 :/\

data Type = Boo
          | Nat
          | Type :-> Type
          | Type :/\ Type
    deriving (Read,Show,Eq)

data Pat = PVar Symb
         | PPair Pat Pat
    deriving (Read,Show,Eq)

data Term = Fls
          | Tru
          | If Term Term Term
          | Zero
          | Succ Term
          | Pred Term
          | IsZero Term
          | Idx Int
          | Term :@: Term
          | Lmb Symb Type Term
          | Let Pat Term Term
          | Pair Term Term
          | Fst Term
          | Snd Term
	  | Fix Term
          deriving (Read,Show)

instance Eq Term where
  Fls       == Fls          =  True
  Tru       == Tru          =  True
  If b u w  == If b1 u1 w1  =  b == b1 && u == u1 && w == w1
  Zero      == Zero         =  True
  Succ u    == Succ u1      =  u == u1
  Pred u    == Pred u1      =  u == u1
  IsZero u  == IsZero u1    =  u == u1
  Idx m     == Idx m1       =  m == m1
  (u:@:w)   == (u1:@:w1)    =  u == u1 && w == w1
  Lmb _ t u == Lmb _ t1 u1  =  t == t1 && u == u1
  Let p u w == Let p1 u1 w1 =  p == p1 && u == u1 && w == w1
  Pair u w  == Pair u1 w1   =  u == u1 && w == w1
  Fst z     == Fst z1       =  z == z1
  Snd z     == Snd z1       =  z == z1
  Fix u     == Fix u1       =  u == u1
  _         == _            =  False

newtype Env = Env [(Symb,Type)]
  deriving (Read,Show,Eq)

patSize :: Pat -> Int
patSize (PVar _) = 1
patSize (PPair p1 p2) = patSize p1 + patSize p2

shift :: Int -> Term -> Term
shift val = shiftAbove 0 where
  shiftAbove cutoff (Idx k) 
                   | k < cutoff = Idx k
                   | otherwise  = Idx $ k + val
  shiftAbove cutoff (t1 :@: t2) = shiftAbove cutoff t1 :@: shiftAbove cutoff t2
  shiftAbove cutoff (Lmb sym tp t) = Lmb sym tp $ shiftAbove (succ cutoff) t 
  shiftAbove cutoff (Let p t1 t2) =
    let [t1', t2'] =  [shiftAbove cutoff t1, shiftAbove (cutoff + patSize p) t2]
    in Let p t1' t2'
  shiftAbove cutoff (If t1 t2 t3) =
    let [t1', t2', t3'] = map (shiftAbove cutoff) [t1, t2, t3]
    in If t1' t2' t3'
  shiftAbove cutoff (Pair t1 t2) = Pair (shiftAbove cutoff t1) (shiftAbove cutoff t2)
  shiftAbove cutoff (Fst t) = Fst $ shiftAbove cutoff t
  shiftAbove cutoff (Snd t) = Snd $ shiftAbove cutoff t
  shiftAbove _ t = t

substDB :: Int -> Term -> Term -> Term
substDB j s (Idx k) 
            | k == j    = s
            | otherwise = Idx k
substDB j s (t1 :@: t2) = substDB j s t1 :@: substDB j s t2
substDB j s (Lmb sym tp t) = Lmb sym tp $ substDB (succ j) (shift 1 s) t
substDB j s (Let p t1 t2) =
    let psz = patSize p
        [t1', t2'] = [substDB j s t1, substDB (j + psz) (shift psz s) t2]
    in Let p t1' t2'
substDB j s (If t1 t2 t3) =
    let [t1', t2', t3'] = map (substDB j s) [t1, t2, t3]
    in If t1' t2' t3'
substDB j s (Pair t1 t2) =
    let [t1', t2'] = map (substDB j s) [t1, t2]
    in Pair t1' t2'
substDB j s (Fst t) = Fst $ substDB j s t
substDB j s (Snd t) = Snd $ substDB j s t
substDB _ _ t = t

betaHelper :: Term -> Term -> Term
betaHelper w u = shift (-1) $ substDB 0 (shift 1 w) u

isValue :: Term -> Bool
isValue Fls = True
isValue Tru = True
isValue (Lmb _ _ _) = True
isValue (Pair t1 t2) = isValue t1 && isValue t2
isValue _ = False

match :: Pat -> Term -> Maybe [(Symb,Term)]
match _ t | not (isValue t) = Nothing
match (PVar s) t = Just [(s, t)]
match (PPair p1 p2) (Pair t1 t2) = do
  ls1 <- match p1 t1
  ls2 <- match p2 t2
  Just $ ls1 ++ ls2
match _ _ = Nothing

oneStep :: Term -> Maybe Term
oneStep u | isValue u = Nothing
oneStep (If Tru t2 _) = Just t2
oneStep (If Fls _ t3) = Just t3
oneStep (If t1 t2 t3) = do
  t1' <- oneStep t1
  Just (If t1' t2 t3)
oneStep (t1@(Lmb _ _ t1') :@: t2)
  | isValue t2 = Just $ betaHelper t2 t1'
  | otherwise = oneStep t2 >>= \t2' -> Just (t1 :@: t2')
oneStep (Let p t1 t2)
  | isValue t1 = do
      lst <- match p t1 
      Just $ foldr betaHelper t2 $ snd $ unzip lst 
  | otherwise = oneStep t1 >>= \t1' -> Just (Let p t1' t2)
oneStep (t1 :@: t2) = oneStep t1 >>= \t1' -> Just (t1' :@: t2)
oneStep (Fst t@(Pair t1 t2)) | isValue t = Just t1
oneStep (Fst t) = oneStep t >>= \t' -> Just (Fst t')
oneStep (Snd t@(Pair t1 t2)) | isValue t = Just t2
oneStep (Snd t) = oneStep t >>= \t' -> Just (Snd t')
oneStep (Pair t1 t2) | isValue t1 = oneStep t2 >>= \t2' -> Just (Pair t1 t2')
oneStep (Pair t1 t2) = oneStep t1 >>= \t1' -> Just (Pair t1' t2)
oneStep _ = Nothing

whnf :: Term -> Term 
whnf u =
  case (oneStep u) of
    Nothing -> u
    (Just u') -> whnf u'

{- Test

cNot = Lmb "x" Boo (If (Idx 0) Fls Tru)
oneStep (cNot :@: Tru) == Just (If Tru Fls Tru)
whnf (cNot :@: Fls) == Tru
whnf (Idx 0 :@: (cNot :@: Tru)) == (Idx 0 :@: (Lmb "x" Boo (If (Idx 0) Fls Tru) :@: Tru))
oneStep (cNot :@: If cNot Tru Fls) == Nothing

test1 = Let "x" Fls $ Lmb "y" (Boo :-> Boo) (Idx 0 :@: Idx 1)
oneStep test1 == Just (Lmb "y" (Boo :-> Boo) (Idx 0 :@: Fls))
test2 = Let "x" (If Tru Fls Tru) $ Lmb "y" (Boo :-> Boo) (Idx 0 :@: Idx 1)
oneStep test2 == Just test1
whnf test2 == Lmb "y" (Boo :-> Boo) (Idx 0 :@: Fls)

cSnd = Lmb "z" (Boo :/\ Boo) (Snd (Idx 0))
cCurry = Lmb "f" (Boo :/\ Boo :-> Boo) $ Lmb "x" Boo $ Lmb "y" Boo $ (Idx 2) :@: Pair (Idx 1) (Idx 0)
whnf (cCurry :@: cSnd :@: Fls :@: Tru) == Tru
whnf (cCurry :@: cSnd :@: Fls) == (Lmb "y" Boo (Lmb "z" (Boo :/\ Boo) (Snd (Idx 0)) :@: Pair Fls (Idx 0)))

[pa,pb,pc,pd] = PVar <$> ["a","b","c","d"]
match (PPair pa pb) (Pair Tru Fls) == Just [("a",Tru),("b",Fls)]
match (PPair (PPair pa pb) pc) (Pair (Pair Tru Fls) Tru) == Just [("a",Tru),("b",Fls),("c",Tru)]
match pa (If Tru Fls Tru) == Nothing
test2 = Let (PPair (PVar "a") (PVar "b")) (Pair Tru Fls) (Idx 2)
substDB 0 (Idx 40) test2 == Let (PPair (PVar "a") (PVar "b")) (Pair Tru Fls) (Idx 42)
test0 = Let (PPair (PVar "a") (PVar "b")) (Pair Tru Fls) (Idx 0)
oneStep test0 == Just Fls
test1 = Let (PPair (PVar "a") (PVar "b")) (Pair Tru Fls) (Idx 1)
oneStep test1 == Just Tru

-}

checkPat :: Pat -> Type -> Maybe Env
checkPat (PVar s) t = Just $ Env [(s,t)]
checkPat (PPair p1 p2) (t1 :/\ t2) = do
  (Env e1) <- checkPat p1 t1
  (Env e2) <- checkPat p2 t2
  Just . Env $ e2 ++ e1
checkPat _ _ = Nothing

infer :: Env -> Term -> Maybe Type
infer env@(Env envLst) u = 
  case u of
    Fls -> Just Boo
    Tru -> Just Boo
    (If t1 t2 t3) -> do
      t1T <- infer env t1
      t2T <- infer env t2
      t3T <- infer env t3
      guard $ t1T == Boo
      guard $ t2T == t3T
      Just t2T
    (Idx n) -> Just $ snd $ envLst !! n
    (Lmb nm xT t) -> do
      tT <- infer (Env ((nm,xT):envLst)) $ t
      Just $ xT :-> tT
    (Let p t1 t2) -> do
      t1T <- infer env t1
      (Env pEnv) <- checkPat p t1T
      infer (Env (pEnv ++ envLst)) $ t2
    (t1 :@: t2) -> do
      t1T <- infer env t1
      t2T <- infer env t2
      case t1T of 
        (xT :-> yT) ->
          if xT == t2T
            then Just yT
            else Nothing
        _ -> Nothing
    (Pair t1 t2) -> do
      t1T <- infer env t1
      t2T <- infer env t2
      Just $ t1T :/\ t2T
    (Fst t) -> do
      (t1T :/\ _) <- infer env t
      Just t1T
    (Snd t) -> do
      (_ :/\ t2T) <- infer env t
      Just t2T


infer0 :: Term -> Maybe Type
infer0 = infer $ Env []

{- Test

cKB = Lmb "x" Boo (Lmb "y" Boo (Idx 1))
infer0 cKB == Just (Boo :-> (Boo :-> Boo))
infer0 (cKB :@: Tru) == Just (Boo :-> Boo)
infer0 (cKB :@: Tru :@: If Tru Fls Tru) == Just Boo
env = Env[("x",Boo),("y",Boo :-> Boo)]
term = Idx 1 :@: Idx 0
infer env term == Just Boo
term = Idx 0 :@: Idx 1
infer env term == Nothing

cK = Lmb "x" Boo (Lmb "y" Boo (Idx 1))
cUnCurry = Lmb "f" (Boo :-> Boo :-> Boo) $ Lmb "z" (Boo :/\ Boo) $ (Idx 1) :@: Fst (Idx 0) :@: Snd (Idx 0)
infer0 (cUnCurry :@: cK) == Just (Boo :/\ Boo :-> Boo)
infer0 (cUnCurry :@: cK :@: Pair Fls Tru) == Just Boo
infer0 (cUnCurry :@: cK :@: Fls) == Nothing

cK = Lmb "x" Boo (Lmb "y" Boo (Idx 1))
cUnCurry' = Lmb "f" (Boo :-> Boo :-> Boo) $ Lmb "z" (Boo :/\ Boo) $ Let (PPair (PVar "x") (PVar "y")) (Idx 0) $ Idx 3 :@: Idx 1 :@: Idx 0
infer0 cUnCurry' == Just ((Boo :-> (Boo :-> Boo)) :-> (Boo :/\ Boo :-> Boo))
infer0 (cUnCurry' :@: cK) == Just (Boo :/\ Boo :-> Boo)
infer0 (cUnCurry' :@: cK :@: Pair Fls Tru) == Just Boo
infer0 (cUnCurry' :@: cK :@: Fls) == Nothing
[pa,pb,pc,pd] = PVar <$> ["a","b","c","d"]
pair  = Pair Tru cK
ppair = PPair pa pb
(infer0 $ Let ppair pair (Idx 0)) == Just (Boo :-> (Boo :-> Boo))
(infer0 $ Let ppair pair (Idx 1)) == Just Boo

-}
