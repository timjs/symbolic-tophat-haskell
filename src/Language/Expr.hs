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

-- Unary -


data Un (a :: Ty) (b :: Ty) where
  Not :: Un 'TyBool 'TyBool

  Fst :: Un (a ':>< b) a
  Snd :: Un (a ':>< b) b


instance Pretty (Un a b) where
  pretty = \case
    Not -> "not"

    Fst -> "fst"
    Snd -> "snd"


un :: Un a b -> TypeOf a -> TypeOf b
un = \case
    Not -> not

    Fst -> fst
    Snd -> snd



-- Binary --


data Bin (a :: Ty) (b :: Ty) (c :: Ty) where
  And :: Bin 'TyBool 'TyBool 'TyBool
  Or  :: Bin 'TyBool 'TyBool 'TyBool

  Lt :: Bin 'TyInt 'TyInt 'TyBool
  Le :: Bin 'TyInt 'TyInt 'TyBool
  Eq :: Bin 'TyInt 'TyInt 'TyBool
  Nq :: Bin 'TyInt 'TyInt 'TyBool
  Ge :: Bin 'TyInt 'TyInt 'TyBool
  Gt :: Bin 'TyInt 'TyInt 'TyBool

  Add :: Bin 'TyInt 'TyInt 'TyInt
  Sub :: Bin 'TyInt 'TyInt 'TyInt
  Mul :: Bin 'TyInt 'TyInt 'TyInt
  Div :: Bin 'TyInt 'TyInt 'TyInt


instance Pretty (Bin a b c) where
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


bin :: Bin a b c -> TypeOf a -> TypeOf b -> TypeOf c
bin = \case
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

  Un :: Un a b -> Expr cxt a -> Expr cxt b
  Bin :: Bin a b c -> Expr cxt a -> Expr cxt b -> Expr cxt c
  If :: Expr cxt 'TyBool -> Expr cxt a -> Expr cxt a -> Expr cxt a

  Unit :: Expr cxt 'TyUnit
  Pair :: Expr cxt a -> Expr cxt b -> Expr cxt (a ':>< b)


instance Pretty (Expr cxt t) where
  pretty = \case
    Lam f -> "λ." <> pretty f
    App f a -> sep [parens (pretty f), parens (pretty a)]
    Var i -> "x" <> pretty i
    Val i -> pretty i

    Un o a -> parens (sep [pretty o, pretty a])
    Bin o a b -> parens (sep [pretty a, pretty o, pretty b])
    If p a b -> sep ["if", pretty p, "then", pretty a, "else", pretty b]

    Unit -> angles neutral
    Pair a b -> angles $ pretty a <> comma <> pretty b


eval :: Env cxt -> Expr cxt t -> TypeOf t
eval env = \case
  Lam f -> \x -> eval (Cons x env) f
  App f a -> eval env f $ eval env a
  Var i -> lookup i env
  Val i -> i

  Un o a -> (un o) (eval env a)
  Bin o a b -> (bin o) (eval env a) (eval env b)
  If p a b -> if eval env p then eval env a else eval env b

  Unit -> ()
  Pair a b -> ( eval env a, eval env b )


eval' :: Expr '[] t -> TypeOf t
eval' = eval Nil



-- Examples --------------------------------------------------------------------


double :: Expr cxt ('TyInt ':-> 'TyInt)
double = Lam (Bin Mul (Val 2) (Var Here))


fact :: Expr cxt ('TyInt ':-> 'TyInt)
fact = Lam
  (If (Bin Eq (Var Here) (Val 0))
    (Val 1)
    (Bin Mul (App fact (Bin Sub (Var Here) (Val 1))) (Var Here)))
