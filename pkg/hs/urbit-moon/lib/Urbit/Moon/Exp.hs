module Urbit.Moon.Exp where

import Urbit.Moon.Prim (Op(..))
import Bound
import Numeric.Natural
import ClassyPrelude
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)


-- Types -----------------------------------------------------------------------

type Nat = Natural

data Exp a
  = Var a
  | App (Exp a) (Exp a)
  | Seq (Exp a) (Exp a)
  | Lam (Scope () Exp a)
  | Let [Scope Int Exp a] (Scope Int Exp a)
  | Con [Nat] [Exp a] [Nat]
  | Pat (Exp a) [(Nat, Scope Int Exp a)]
  | Opr (Op (Exp a))
 deriving (Functor, Foldable, Traversable)


-- Instances -------------------------------------------------------------------

deriveEq1   ''Exp
deriveOrd1  ''Exp
deriveRead1 ''Exp
deriveShow1 ''Exp

deriving instance Eq a => Eq (Exp a)
deriving instance Ord a => Ord (Exp a)
deriving instance Show a => Show (Exp a)

instance Applicative Exp where
  pure = Var
  (<*>) = ap

instance Monad Exp where
  return = Var

  Var a     >>= f = f a
  Lam b     >>= f = Lam (b >>>= f)
  Let x b   >>= f = Let (map (>>>= f) x) (b >>>= f)
  App x y   >>= f = App (x >>= f) (y >>= f)
  Seq x y   >>= f = Seq (x >>= f) (y >>= f)
  Con l x r >>= f = Con l (map (>>= f) x) r
  Pat x p   >>= f = Pat (x >>= f) $ map (\(x,y) -> (x, y >>>= f)) p
  Opr x     >>= f = Opr ((>>= f) <$> x)
