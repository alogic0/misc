module HindleyMilner where
import Control.Applicative ((<|>))
import Control.Monad (guard)

type Symb = String

infixr 3 :->

data Type
  = TVar Symb
  | Type :-> Type
  deriving (Eq, Show)
  
subst :: [(Type,Type)] -> Type -> Maybe Type
subst lst t =
  case t of
    (t1 :-> t2) -> (:->) <$> (subst lst t1) <*> (subst lst t2)
    _           -> lookup t lst <|> Just t

freeVars :: Type -> [Symb]
freeVars (TVar x) = [x]
freeVars (t1 :-> t2) = freeVars t1 ++ freeVars t2

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
  s1' <- subst uni2 s1
  t1' <- subst uni2 t1
  uniNew <- unify s1' t1'
  return $ uni2 ++ uniNew

-- Tests
{-
test a b = unify a b >>= \lst -> return $ subst lst a == subst lst b
test (TVar "a" :-> TVar "b" :-> TVar "c") (TVar "d" :-> TVar "d")
test (TVar "b" :-> TVar "b") (((TVar "g" :-> TVar "d") :-> TVar "e") :-> TVar "a" :-> TVar "d")
-}
