{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Moon.CAF
  ( CAF(..)
  , Prog(..)
  ) where

import Bound
import ClassyPrelude

import Data.Deriving   (deriveEq1, deriveOrd1)
import Urbit.Moon.Atom (Atom(..))
import Urbit.Moon.Prim (Op(..))


-- Types -----------------------------------------------------------------------

data Prog = Prog [(Text, Int, CAF (Var Int Text))]

data CAF a
  = CVar a
  | CApp (CAF a) (CAF a)
  | CSeq (CAF a) (CAF a)
  | COpr (Op (CAF a))
  | CLit Atom
  | CVec [CAF a]
--  CCon [Int] Int [CAF a]
--  CCas (CAF a) [(Maybe Atom, CAF a)]
--  CPat (CAF a) [(Int, Scope Int CAF a)]
 deriving (Functor, Foldable, Traversable)


-- Instances -------------------------------------------------------------------

deriveEq1   ''CAF
deriveOrd1  ''CAF

deriving instance Eq a => Eq (CAF a)
deriving instance Ord a => Ord (CAF a)

instance Applicative CAF where
  pure = CVar
  (<*>) = ap

instance Monad CAF where
  return = CVar

  CVar a      >>= f = f a
  CApp x y    >>= f = CApp (x >>= f) (y >>= f)
  COpr x      >>= f = COpr ((>>= f) <$> x)
  CLit l      >>= _ = CLit l
  CVec xs     >>= f = CVec ((>>= f) <$> xs)
  CSeq x y    >>= f = CSeq (x >>= f) (y >>= f)
--CCon ns n x >>= f = CCon ns n ((>>= f) <$> x)
--CPat v p    >>= f = CPat (v >>= f) (over _2 (>>>= f) <$> p)
--CCas x cs   >>= f = CCas (x >>= f) (over _2 (>>= f) <$> cs)
