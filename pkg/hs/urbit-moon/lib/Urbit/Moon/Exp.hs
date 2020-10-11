module Urbit.Moon.Exp where

import Urbit.Moon.Prim (Op(..))
import Urbit.Moon.Lit (Literal(..))
import Bound
import Numeric.Natural
import ClassyPrelude
import Data.Deriving (deriveEq1, deriveOrd1)
import Control.Lens (over, _2)


-- Types -----------------------------------------------------------------------

type Nat = Natural

type Typ = Exp

{-
  Lazy Dependant Lambda Calculus with `seq` and primitives.

  Primitives:

  - Dat: Structural ADTs
  - Nat: Natural
  - Int: Integer
  - Vec: Array
  - Wor: Words -- arrays of bits of a certain width.
  - Buf: Packed Arrays of words.

  - Note that Let supports recursive bindings.
  - Note that the type of `Typ n` is `Typ (n+1)`.
  - Note that functions take multiple arguments. This enables better
    codegen to hoon/scheme/js.

-}
data Exp a
  = Var a
  | App (Exp a) (Exp a)
  | Lam (Scope () Exp a)
  | Seq (Exp a) (Exp a)
  | Let [Scope Int Typ a] -- types
        [Scope Int Exp a] -- values
        (Scope Int Exp a) -- body

  -- Types
  | Typ Nat             -- Type of (Typ n) is (Typ (n+1))
  | Fun (Typ a) (Scope () Exp a)
  | Dat [[Typ a]]       -- Structural ADTs
  | Nat                 -- Natural
  | Int                 -- Integer
  | Vec (Typ a)         -- Array
  | Wor Nat             -- Word with bit-width n
  | Buf Nat             -- Packed vector of words of bit-width n

  | Con (Typ a) Nat [Exp a]                -- Construct ADT
  | Pat (Exp a) (Typ a) [(Int, Scope Int Exp a)] -- Pattern match on ADT
  | Opr (Op (Exp a))                       -- Operations on primitives
  | Lit Literal
 deriving (Functor, Foldable, Traversable)

maybeTy :: Typ a
maybeTy = Lam (Scope $ Dat [[], [Var (B ())]])


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

  Var a     >>= f = f a
  Lam b     >>= f = Lam (b >>>= f)
  Let t x b >>= f = Let ((>>>= f) <$> t) ((>>>= f) <$> x) (b >>>= f)
  App x y   >>= f = App (x >>= f) (y >>= f)
  Seq x y   >>= f = Seq (x >>= f) (y >>= f)
  Con t c x >>= f = Con (t >>= f) c ((>>= f) <$> x)
  Pat v t p >>= f = Pat (v >>= f) (t >>= f) (over _2 (>>>= f) <$> p)
  Opr x     >>= f = Opr ((>>= f) <$> x)
  Lit l     >>= _ = Lit l
  Typ x     >>= _ = Typ x
  Fun x y   >>= f = Fun (x >>= f) (y >>>= f)
  Dat x     >>= f = Dat (fmap (>>= f) <$> x)
  Nat       >>= f = Nat
  Int       >>= f = Int
  Vec x     >>= f = Vec (x >>= f)
  Wor n     >>= _ = Wor n
  Buf n     >>= _ = Buf n
