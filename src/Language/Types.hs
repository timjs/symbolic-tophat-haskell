module Language.Types
  ( module Data.Universe
  , Ty(..)
  ) where


import Preload

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
