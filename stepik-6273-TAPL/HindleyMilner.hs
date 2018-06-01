module HindleyMilner where
import Control.Applicative

type Symb = String

infixr 3 :->

data Type
  = TVar Symb
  | Type :-> Type
  deriving (Eq, Show)
  
subst :: (Type -> Maybe Type) -> Type -> Maybe Type
subst s t =
  case t of
    (t1 :-> t2) -> (:->) <$> (subst s' t1) <*> (subst s' t2)
    _           -> s' t
  where s' t = s t <|> Just t

freeVars :: Type -> [Symb]
freeVars (TVar x) = [x]
freeVars (t1 :-> t2) = freeVars t1 ++ freeVars t2

mkFun :: [(Type,Type)] -> Type -> Maybe Type
mkFun lst t = lookup t lst <|> Just t

unify :: Type -> Type -> Maybe [(Type, Type)]
unify  (TVar a) (TVar b) =
  if a == b
  then Just [] 
  else Nothing
unify va@(TVar a) t
  | a `elem` freeVars t = Nothing
  | otherwise = Just [(va,t)]
unify t@(_ :-> _) va@(TVar _) = unify va t
unify (s1 :-> s2) (t1 :-> t2) = do
  uni2 <- unify s2 t2
  s1' <- mkFun uni2 s1
  t1' <- mkFun uni2 t1
  uniNew <- unify s1' t1'
  return $ uni2 ++ uniNew

-- Tests
{-
test a b = unify a b >>= \lst -> return $ subst (mkFun lst) a == subst (mkFun lst) b
test (TVar "a" :-> TVar "b" :-> TVar "c") (TVar "d" :-> TVar "d")
-}
