module Language.Ops where


import Language.Types



-- Operations ------------------------------------------------------------------

-- Unary -


data Un (a :: PrimTy) (b :: PrimTy) where
  Not :: Un 'TyBool 'TyBool
  Neg :: Un 'TyInt  'TyInt


instance Pretty (Un a b) where
  pretty = \case
    Not -> "not"
    Neg -> "neg"



-- Binary --


data Bn (a :: PrimTy) (b :: PrimTy) (c :: PrimTy) where
  Conj :: Bn 'TyBool 'TyBool 'TyBool
  Disj :: Bn 'TyBool 'TyBool 'TyBool

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
    Conj -> "∧"
    Disj -> "∨"

    Lt -> "<"
    Le -> "≤"
    Eq -> "≡"
    Nq -> "≢"
    Ge -> "≥"
    Gt -> ">"

    Add -> "+"
    Sub -> "-"
    Mul -> "×"
    Div -> "÷"
