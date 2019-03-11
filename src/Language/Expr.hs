module Language.Expr where


import Preload

import Data.Text.Prettyprint.Doc
import Data.Universe



-- Universe --------------------------------------------------------------------


data {- kind -} Ty
  = TyInt
  | TyBool
  | Ty :-> Ty


instance Universe Ty where
  type TypeOf 'TyInt = Int
  type TypeOf 'TyBool = Bool
  type TypeOf (a ':-> b) = TypeOf a -> TypeOf b



-- Context ---------------------------------------------------------------------


data HasType (cxt :: List Ty) (t :: Ty) where
  Here :: HasType (t ': ts) t
  There :: HasType ts t -> HasType (t2 ': ts) t


data Env (cxt :: List Ty) where
  Nil :: Env '[]
  Cons :: TypeOf t -> Env ts -> Env (t ': ts)


lookup :: HasType cxt t -> Env cxt -> TypeOf t
lookup Here      (Cons x _)  = x
lookup (There i) (Cons _ xs) = lookup i xs


count :: HasType cxt t -> Int
count = \case
  Here -> 0
  There xs -> 1 + count xs


instance Pretty (HasType cxt t) where
  pretty = pretty << count



-- Expressions -----------------------------------------------------------------


data Expr (cxt :: List Ty) (t :: Ty) where
  Val :: Int -> Expr cxt 'TyInt
  Var :: HasType cxt t -> Expr cxt t
  Lam :: Expr (a ': cxt) t -> Expr cxt (a ':-> t)
  Op :: (TypeOf a -> TypeOf b -> TypeOf c) -> Expr cxt a -> Expr cxt b -> Expr cxt c
  App :: Expr cxt (a ':-> b) -> Expr cxt a -> Expr cxt b
  If :: Expr cxt 'TyBool -> Expr cxt a -> Expr cxt a -> Expr cxt a


instance Pretty (Expr cxt t) where
  pretty = \case
    Val i -> pretty i
    Op _ a b -> parens (sep [pretty a, "<>", pretty b])
    Var i -> "x" <> pretty i
    Lam f -> "λ." <> pretty f
    App f a -> sep [parens (pretty f), parens (pretty a)]
    If p a b -> sep ["if", pretty p, "then", pretty a, "else", pretty b]


eval :: Env cxt -> Expr cxt t -> TypeOf t
eval env = \case
  Val i -> i
  Op f a b -> f (eval env a) (eval env b)
  Var i -> lookup i env
  Lam f -> \x -> eval (Cons x env) f
  App f a -> eval env f $ eval env a
  If p a b -> if eval env p then eval env a else eval env b


eval' :: Expr '[] t -> TypeOf t
eval' = eval Nil



-- Examples --------------------------------------------------------------------


double :: Expr cxt ('TyInt ':-> 'TyInt)
double = Lam (Op (*) (Val 2) (Var Here))


fact :: Expr cxt ('TyInt ':-> 'TyInt)
fact = Lam
  (If (Op (==) (Var Here) (Val 0))
    (Val 1)
    (Op (*) (App fact (Op (-) (Var Here) (Val 1))) (Var Here)))
