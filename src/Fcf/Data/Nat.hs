{-# LANGUAGE
    DataKinds,
    PolyKinds,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    UndecidableInstances #-}

-- | Natural numbers.
--
-- Note that the operators from this module conflict with "GHC.TypeLits" and
-- "GHC.TypeNats".
module Fcf.Data.Nat
  ( -- * Reexported type
    -- | From "GHC.TypeNats".

    Nat

    -- * Operations

  , type (+)
  , type (-)
  , type (Fcf.Data.Nat.*)
  , type (^)
  , type (<=)
  , type (>=)
  , type (<)
  , type (>)
  , Min
  , Max
  ) where

import GHC.TypeLits (Nat)
import qualified GHC.TypeLits as TL

import Fcf.Core
import Fcf.Combinators
import Fcf.Data.Bool (Not)
import Fcf.Utils (If)

data (+) :: Nat -> Nat -> Exp Nat
type instance Eval ((+) a b) = a TL.+ b

data (-) :: Nat -> Nat -> Exp Nat
type instance Eval ((-) a b) = a TL.- b

data (*) :: Nat -> Nat -> Exp Nat
type instance Eval ((Fcf.Data.Nat.*) a b) = a TL.* b

data (^) :: Nat -> Nat -> Exp Nat
type instance Eval ((^) a b) = a TL.^ b

data (<=) :: Nat -> Nat -> Exp Bool
type instance Eval ((<=) a b) = a TL.<=? b

data (>=) :: Nat -> Nat -> Exp Bool
type instance Eval ((>=) a b) = b TL.<=? a

data (<) :: Nat -> Nat -> Exp Bool
type instance Eval ((<) a b) = Eval (Not =<< (a >= b))

data (>) :: Nat -> Nat -> Exp Bool
type instance Eval ((>) a b) = Eval (Not =<< (a <= b))

data Min :: Nat -> Nat -> Exp Nat
type instance Eval (Min a b) = If (a TL.<=? b) a b

data Max :: Nat -> Nat -> Exp Nat
type instance Eval (Max a b) = If (a TL.<=? b) b a
