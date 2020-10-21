{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Moon.Vulgar
  ( Vul(..)
  , VulJet(..)
  ) where

import ClassyPrelude
import Numeric.Natural


-- Types -----------------------------------------------------------------------

data VulJet = VulJet
  { vjArgs :: Natural
  , vjName :: Natural
  , vjbody :: Vul (Either Int VulJet)
  }

data Vul a
  = Var a
  | App (Vul a) (Vul a)
  | Seq (Vul a) (Vul a)
  | Jet VulJet

  | Nat Natural
  | NatInc (Vul a)
  | NatDec (Vul a)
  | NatAdd (Vul a) (Vul a)
  | NatMul (Vul a) (Vul a)

  | Vec (Vector (Vul a))
  | VecIdx (Vul a) (Vul a)
  | VecSet (Vul a) (Vul a) (Vul a)
  | VecMap (Vul a) (Vul a)
 deriving (Functor, Foldable, Traversable)


-- Instances -------------------------------------------------------------------

instance Applicative Vul where
  pure = Var
  (<*>) = ap

instance Monad Vul where
  return = Var

  Var a        >>= f = f a
  App x y      >>= f = App (x >>= f) (y >>= f)
  Seq x y      >>= f = Seq (x >>= f) (y >>= f)
  Jet x        >>= f = error "TODO" x f
  Nat n        >>= _ = Nat n
  NatInc x     >>= f = NatInc (x >>= f)
  NatDec x     >>= f = NatDec (x >>= f)
  NatAdd x y   >>= f = NatAdd (x >>= f) (y >>= f)
  NatMul x y   >>= f = NatMul (x >>= f) (y >>= f)
  Vec xs       >>= f = Vec ((>>= f) <$> xs)
  VecIdx x i   >>= f = VecIdx (x >>= f) (i >>= f)
  VecSet x i v >>= f = VecSet (x >>= f) (i >>= f) (v >>= f)
  VecMap x i   >>= f = VecMap (x >>= f) (i >>= f)
