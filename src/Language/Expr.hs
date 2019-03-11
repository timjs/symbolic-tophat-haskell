module Language.Expr where


import Preload

import Data.Text.Prettyprint.Doc

import Language.Types



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



-- Operations ------------------------------------------------------------------


data Op (a :: Ty) (b :: Ty) (c :: Ty) where
  And :: Op 'TyBool 'TyBool 'TyBool
  Or  :: Op 'TyBool 'TyBool 'TyBool
  -- Not :: Op TyBool TyUnit TyBool

  Lt :: Op 'TyInt 'TyInt 'TyBool
  Le :: Op 'TyInt 'TyInt 'TyBool
  Eq :: Op 'TyInt 'TyInt 'TyBool
  Nq :: Op 'TyInt 'TyInt 'TyBool
  Ge :: Op 'TyInt 'TyInt 'TyBool
  Gt :: Op 'TyInt 'TyInt 'TyBool

  Add :: Op 'TyInt 'TyInt 'TyInt
  Sub :: Op 'TyInt 'TyInt 'TyInt
  Mul :: Op 'TyInt 'TyInt 'TyInt
  Div :: Op 'TyInt 'TyInt 'TyInt


instance Pretty (Op a b c) where
  pretty = \case
    And -> "&&"
    Or  -> "||"

    Lt -> "<"
    Le -> "<="
    Eq -> "=="
    Nq -> "/="
    Ge -> ">="
    Gt -> ">"

    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"


op :: Op a b c -> TypeOf a -> TypeOf b -> TypeOf c
op = \case
  And -> (&&)
  Or  -> (||)

  Lt -> (<)
  Le -> (<=)
  Eq -> (==)
  Nq -> (/=)
  Ge -> (>=)
  Gt -> (>)

  Add -> (+)
  Sub -> (-)
  Mul -> (*)
  Div -> div



-- Expressions -----------------------------------------------------------------


data Expr (cxt :: List Ty) (t :: Ty) where
  Lam :: Expr (a ': cxt) t -> Expr cxt (a ':-> t)
  App :: Expr cxt (a ':-> b) -> Expr cxt a -> Expr cxt b
  Var :: HasType cxt t -> Expr cxt t
  Val :: Pretty (TypeOf a) => TypeOf a -> Expr cxt a

  Op :: Op a b c -> Expr cxt a -> Expr cxt b -> Expr cxt c
  If :: Expr cxt 'TyBool -> Expr cxt a -> Expr cxt a -> Expr cxt a

  Unit :: Expr cxt 'TyUnit
  Pair :: Expr cxt a -> Expr cxt b -> Expr cxt (a ':>< b)


instance Pretty (Expr cxt t) where
  pretty = \case
    Lam f -> "λ." <> pretty f
    App f a -> sep [parens (pretty f), parens (pretty a)]
    Var i -> "x" <> pretty i
    Val i -> pretty i

    Op o a b -> parens (sep [pretty a, pretty o, pretty b])
    If p a b -> sep ["if", pretty p, "then", pretty a, "else", pretty b]

    Unit -> angles neutral
    Pair a b -> angles $ pretty a <> comma <> pretty b


eval :: Env cxt -> Expr cxt t -> TypeOf t
eval env = \case
  Lam f -> \x -> eval (Cons x env) f
  App f a -> eval env f $ eval env a
  Var i -> lookup i env
  Val i -> i

  Op o a b -> (op o) (eval env a) (eval env b)
  If p a b -> if eval env p then eval env a else eval env b

  Unit -> ()
  Pair a b -> ( eval env a, eval env b )


eval' :: Expr '[] t -> TypeOf t
eval' = eval Nil



-- Examples --------------------------------------------------------------------


double :: Expr cxt ('TyInt ':-> 'TyInt)
double = Lam (Op Mul (Val 2) (Var Here))


fact :: Expr cxt ('TyInt ':-> 'TyInt)
fact = Lam
  (If (Op Eq (Var Here) (Val 0))
    (Val 1)
    (Op Mul (App fact (Op Sub (Var Here) (Val 1))) (Var Here)))
