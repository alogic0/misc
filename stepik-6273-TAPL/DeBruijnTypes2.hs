import Control.Monad

type Symb = String
infixl 2 :@: 
infixr 3 :->

data Type = Boo
          | Type :-> Type
    deriving (Read,Show,Eq)

data Term = Fls
          | Tru
          | If Term Term Term
          | Idx Int
          | Term :@: Term
          | Lmb Symb Type Term
          | Let Symb Term Term
          deriving (Read,Show)

instance Eq Term where
  Fls       == Fls         =  True
  Tru       == Tru         =  True
  If b u w  == If b1 u1 w1 =  b == b1 && u == u1 && w == w1
  Idx m     == Idx m1      =  m == m1
  (u:@:w)   == (u1:@:w1)   =  u == u1 && w == w1
  Lmb _ t u == Lmb _ t1 u1 =  t == t1 && u == u1
  Let _ u w == Let _ u1 w1 =  u == u1 && w == w1
  _         == _           =  False

newtype Env = Env [(Symb,Type)]
  deriving (Read,Show,Eq)

shift :: Int -> Term -> Term
shift val = shiftAbove 0 where
  shiftAbove cutoff (Idx k) 
                   | k < cutoff = Idx k
                   | otherwise  = Idx $ k + val
  shiftAbove cutoff (t1 :@: t2) = shiftAbove cutoff t1 :@: shiftAbove cutoff t2
  shiftAbove cutoff (Lmb sym tp t) = Lmb sym tp $ shiftAbove (succ cutoff) t 
  shiftAbove cutoff (Let sym t1 t2) =
    let [t1', t2'] = map (shiftAbove cutoff) [t1, t2]
    in Let sym t1' t2'
  shiftAbove cutoff (If t1 t2 t3) =
    let [t1', t2', t3'] = map (shiftAbove cutoff) [t1, t2, t3]
    in If t1' t2' t3'
  shiftAbove _ t = t

substDB :: Int -> Term -> Term -> Term
substDB j s (Idx k) 
            | k == j    = s
            | otherwise = Idx k
substDB j s (t1 :@: t2) = substDB j s t1 :@: substDB j s t2
substDB j s (Lmb sym tp t) = Lmb sym tp $ substDB (succ j) (shift 1 s) t
substDB j s (Let sym t1 t2) =
    let [t1', t2'] = map (substDB j s) [t1, t2]
    in Let sym t1' t2'
substDB j s (If t1 t2 t3) =
    let [t1', t2', t3'] = map (substDB j s) [t1, t2, t3]
    in If t1' t2' t3'
substDB _ _ t = t

isValue :: Term -> Bool
isValue Fls = True
isValue Tru = True
isValue (Lmb _ _ _) = True
isValue _ = False

oneStep :: Term -> Maybe Term
oneStep u | isValue u = Nothing
oneStep (If Tru t2 _) = Just t2
oneStep (If Fls _ t3) = Just t3
oneStep (If t1 t2 t3) = do
  t1' <- oneStep t1
  return (If t1' t2 t3)
oneStep (t1@(Lmb _ _ t1') :@: t2)
  | isValue t2 = return $ shift (-1) $ substDB 0 (shift 1 t2) t1'
  | otherwise = oneStep t2 >>= \t2' -> return (t1 :@: t2')
oneStep (Let sym t1 t2)
  | isValue t1 = return $ substDB 0 t1 t2
  | otherwise = oneStep t1 >>= \t1' -> return (Let sym t1' t2)
oneStep (t1 :@: t2) = oneStep t1 >>= \t1' -> return (t1' :@: t2)
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

-}



infer :: Env -> Term -> Maybe Type
infer env@(Env envLst) u = 
  case u of
    Fls -> return Boo
    Tru -> return Boo
    (If t1 t2 t3) -> do
      t1T <- infer env t1
      t2T <- infer env t2
      t3T <- infer env t3
      guard $ t1T == Boo
      guard $ t2T == t3T
      return t2T
    (Idx n) -> return $ snd $ envLst !! n
    (Lmb nm xT t) -> do
      tT <- infer (Env ((nm,xT):envLst)) $ t
      return $ xT :-> tT
    (Let nm t1 t2) -> do
      t1T <- infer env t1
      infer (Env ((nm,t1T):envLst)) $ t2
    (t1 :@: t2) -> do
      t1T <- infer env t1
      t2T <- infer env t2
      case t1T of 
        (xT :-> yT) ->
          if xT == t2T
            then return yT
            else Nothing
        _ -> Nothing


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

test = Let "x" Fls $ Lmb "y" (Boo :-> Boo) (Idx 0 :@: Idx 1)
infer0 test == Just ((Boo :-> Boo) :-> Boo)

-}
