module Urbit.Moon.Exp where

import Bound
import ClassyPrelude
import Control.Lens    (over, _2)
import Data.Deriving   (deriveEq1, deriveOrd1)
import Numeric.Natural
import Urbit.Moon.Atom (Atom(..))
import Urbit.Moon.Prim (Op(..))


-- Types -----------------------------------------------------------------------

type Nat = Natural

data Exp a
  = Var a
  | App (Exp a) (Exp a)
  | Lam (Scope () Exp a)
  | Let [Scope Int Exp a] (Scope Int Exp a)
  | Con [Int] Int [Exp a]
  | Pat (Exp a) [(Int, Scope Int Exp a)]
  | Seq (Exp a) (Exp a)
  | Lit Atom
  | Cas (Exp a) [(Maybe Atom, Exp a)]
  | Vec [Exp a]
  | Opr (Op (Exp a))
 deriving (Functor, Foldable, Traversable)


-- Instances -------------------------------------------------------------------

deriveEq1   ''Exp
deriveOrd1  ''Exp

deriving instance Eq a => Eq (Exp a)
deriving instance Ord a => Ord (Exp a)

instance Applicative Exp where
  pure = Var
  (<*>) = ap

instance Monad Exp where
  return = Var

  Var a      >>= f = f a
  Lam b      >>= f = Lam (b >>>= f)
  Let x b    >>= f = Let ((>>>= f) <$> x) (b >>>= f)
  App x y    >>= f = App (x >>= f) (y >>= f)
  Con ns n x >>= f = Con ns n ((>>= f) <$> x)
  Pat v p    >>= f = Pat (v >>= f) (over _2 (>>>= f) <$> p)
  Opr x      >>= f = Opr ((>>= f) <$> x)
  Lit l      >>= _ = Lit l
  Vec xs     >>= f = Vec ((>>= f) <$> xs)
  Seq x y    >>= f = Seq (x >>= f) (y >>= f)
  Cas x cs   >>= f = Cas (x >>= f) (over _2 (>>= f) <$> cs)
