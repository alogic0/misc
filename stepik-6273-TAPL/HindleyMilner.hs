module HindleyMilner where

type Symb = String

infixr 3 :->

data Type
  = TVar Symb
  | Type :-> Type
  deriving (Eq, Show)
  
exts :: (Type -> Maybe Type) -> Type -> Maybe Type
exts s t =
  case (s t) of
    Nothing -> Just t
    x       -> x

subst :: (Type -> Maybe Type) -> Type -> Maybe Type
subst s t =
  case t of
    (t1 :-> t2) -> (:->) <$> (subst s' t1) <*> (subst s' t2)
    _           -> s' t
  where s' = exts s
