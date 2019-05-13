module Language.Op where


import Language.Type



-- Operations ------------------------------------------------------------------

-- Unary -


data Un (a :: PrimTy) (b :: PrimTy) where
  Not :: Un 'TyBool   'TyBool
  Neg :: Un 'TyInt    'TyInt
  Len :: Un 'TyString 'TyInt


instance Pretty (Un a b) where
  pretty = \case
    Not -> "not"
    Neg -> "neg"
    Len -> "len"


instance Eq (Un a b) where
  Not == Not = True
  Neg == Neg = True
  Len == Len = True
  _   == _   = False



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

  Cat :: Bn 'TyString 'TyString 'TyString


instance Pretty (Bn a b c) where
  pretty = \case
    Conj -> "∧"
    Disj -> "∨"

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

    Cat -> "++"


instance Eq (Bn a b c) where
    Conj == Conj = True
    Disj == Disj = True

    Lt   == Lt = True
    Le   == Le = True
    Eq   == Eq = True
    Nq   == Nq = True
    Ge   == Ge = True
    Gt   == Gt = True

    Add  == Add = True
    Sub  == Sub = True
    Mul  == Mul = True
    Div  == Div = True

    Cat  == Cat = True

    _    == _   = False
