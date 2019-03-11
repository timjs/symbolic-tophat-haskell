module Expr where


import Preload



data Ty
  = TyInt
  | TyBool
  | Ty :-> Ty


type family Interp (t :: Ty) where
  Interp 'TyInt     = Int
  Interp 'TyBool    = Bool
  Interp (a ':-> b) = Interp a -> Interp b


data HasType (cxt :: List Ty) (t :: Ty) where
  Here  :: HasType (t ': ts) t
  There :: HasType ts t -> HasType (t2 ': ts) t


data Expr (cxt :: List Ty) (t :: Ty) where
  Val :: Int -> Expr cxt 'TyInt
  Var :: HasType cxt t -> Expr cxt t
  Lam :: Expr (a ': cxt) t -> Expr cxt (a ':-> t)
  Op  :: (Interp a -> Interp b -> Interp c) -> Expr cxt a -> Expr cxt b -> Expr cxt c
  App :: Expr cxt (a ':-> b) -> Expr cxt a -> Expr cxt b
  If  :: Expr cxt 'TyBool -> Expr cxt a -> Expr cxt a -> Expr cxt a


data Env (cxt :: List Ty) where
  Nil  :: Env '[]
  Cons :: Interp t -> Env ts -> Env (t ': ts)


lookupVar :: HasType cxt t -> Env cxt -> Interp t
lookupVar Here     (Cons x _) = x
lookupVar (There i) (Cons _ xs) = lookupVar i xs


eval :: Env cxt -> Expr cxt t -> Interp t
eval env = \case
  Val i    -> i
  Op f a b -> f (eval env a) (eval env b)
  Var i    -> lookupVar i env
  Lam f    -> \x -> eval (Cons x env) f
  App f a  -> eval env f $ eval env a
  If p a b -> if eval env p then eval env a else eval env b


eval' :: Expr '[] t -> Interp t
eval' = eval Nil
