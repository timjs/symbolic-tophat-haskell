module Language.Ops where


import Language.Types



-- Operations ------------------------------------------------------------------

-- Unary -


data Un (a :: Ty) (b :: Ty) where
  Not :: Un 'TyBool 'TyBool
  Neg :: Un 'TyInt  'TyInt


instance Pretty (Un a b) where
  pretty = \case
    Not -> "not"
    Neg -> "neg"



-- Binary --


data Bn (a :: Ty) (b :: Ty) (c :: Ty) where
  And :: Bn 'TyBool 'TyBool 'TyBool
  Or  :: Bn 'TyBool 'TyBool 'TyBool

  Lt :: Bn 'TyInt 'TyInt 'TyBool
  Le :: Bn 'TyInt 'TyInt 'TyBool
  Eq :: Bn 'TyInt 'TyInt 'TyBool
  Nq :: Bn 'TyInt 'TyInt 'TyBool
  Ge :: Bn 'TyInt 'TyInt 'TyBool
  Gt :: Bn 'TyInt 'TyInt 'TyBool

  Add :: Bn 'TyInt 'TyInt 'TyInt
  Sub :: Bn 'TyInt 'TyInt 'TyInt
  Mul :: Bn 'TyInt 'TyInt 'TyInt
  Div :: Bn 'TyInt 'TyInt 'TyInt


instance Pretty (Bn a b c) where
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
