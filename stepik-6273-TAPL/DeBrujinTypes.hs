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
          deriving (Read,Show)

instance Eq Term where
  Fls       == Fls         =  True
  Tru       == Tru         =  True
  If b u w  == If b1 u1 w1 =  b == b1 && u == u1 && w == w1
  Idx m     == Idx m1      =  m == m1
  (u:@:w)   == (u1:@:w1)   =  u == u1 && w == w1
  Lmb _ t u == Lmb _ t1 u1 =  t == t1 && u == u1
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

oneStep _ = undefined

whnf :: Term -> Term 
whnf u = undefined
