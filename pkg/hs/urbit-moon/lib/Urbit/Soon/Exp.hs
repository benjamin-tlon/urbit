{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Soon.Exp
  ( Bin(..)
  , Exp(..)
  , Fun(..)
  , funBin
  , run
  , nf
  , whnf
  , showSkew
  ) where

import ClassyPrelude
import Bound
import Bound.Var
import Numeric.Natural
import Data.Void
import Data.Tree


-- Types -----------------------------------------------------------------------

type Nat = Natural

infixl 5 :@;

data Bin = N Nat | Bin :@ Bin
 deriving (Eq, Ord)

data Fun = Fun { fArgs :: Nat, fName :: Nat, fBody :: Exp }
 deriving (Eq, Ord, Show)

data Exp
  = Var Nat
  | Jet Fun
  | App Exp Exp
  | Lit Nat
 deriving (Eq, Ord, Show)


-- Instances -------------------------------------------------------------------

tree :: Bin -> Tree Nat
tree = go [] where go a = \case { N n -> Node n a; x :@ y -> go (tree y:a) x }

showTree :: Show a => Tree a -> String
showTree (Node n []) = show n
showTree (Node n xs) = "(" <> intercalate " " (show n : fmap showTree xs) <> ")"

instance Show Bin where show = showTree . tree


-- Compile to Noun -------------------------------------------------------------

funBin :: Fun -> Bin
funBin fun@Fun{..} = N 3 :@ N fArgs :@ N fName :@ brak (funLam fun)

{-
  TODO Can probably skip this conversion by rewritting `brak` in
  terms of `Exp`.
-}
funLam :: Fun -> Lam Void
funLam Fun{..} = wrap fArgs (go fBody)
 where
  go :: Exp -> Lam Nat
  go = \case Var n   -> V n
             Jet f   -> P (funBin f)
             App x y -> A (go x) (go y)
             Lit n   -> P (N n)

  badRef :: Nat -> void
  badRef = error . ("Invalid reference index: " <>) . show

  decRef :: Nat -> Nat -> Var () Nat
  decRef n i = case compare i (n-1) of { EQ -> B (); LT -> F i; GT -> badRef i }

  wrap :: Nat -> Lam Nat -> Lam Void
  wrap = \args top -> badRef <$> f args top
   where
    f :: Nat -> Lam Nat -> Lam Nat
    f 0 l = l
    f n l = f (n-1) $ L $ Scope $ (fmap V . decRef n) <$> l


-- Bracket Abstraction ---------------------------------------------------------

data Lam a
  = L (Scope () Lam a)
  | V a
  | A (Lam a) (Lam a)
  | P Bin
 deriving (Functor, Foldable, Traversable)

instance Applicative Lam where
  pure  = V
  (<*>) = ap

instance Monad Lam where
  return = V
  V a   >>= f = f a
  P p   >>= _ = P p
  L b   >>= f = L (b >>>= f)
  A x y >>= f = A (x >>= f) (y >>= f)

brak :: Lam Void -> Bin
brak = done . go
 where
  done :: Lam Void -> Bin
  done (L _)   = error "impossible in brak.done"
  done (V v)   = absurd v
  done (A x y) = done x :@ done y
  done (P v)   = v

  (s,k) = (P (N 0), P (N 1))

  go :: Eq a => Lam a -> Lam a
  go (V a)   = V a
  go (P n)   = P n
  go (A x y) = A (go x) (go y)
  go (L b)   = ab $ go $ fromScope b

  ab :: Eq a => Lam (Var () a) -> Lam a
  ab (L _)             = error "impossible in bracked.ab"
  ab (P p)             = P p
  ab (V (B ()))        = s `A` k `A` k
  ab (V (F v))         = k `A` V v -- Just to satisfy pattern match checker.
  ab (strip -> Just b) = k `A` b
  ab (A x y)           = s `A` ab x `A` ab y

  strip :: Traversable f => f (Var b a) -> Maybe (f a)
  strip = traverse (unvar (const Nothing) Just)


-- SKEW Evaluation -------------------------------------------------------------

-- Arguments stored in reverse order
data Leaf = T Nat | J Nat Nat (Tree Leaf)

instance Show Leaf where
  show (T n)     = show n
  show (J n t b) = "{" <> intercalate " " [show n, show t, show b] <> "}"

showSkew :: Tree Leaf -> String
showSkew (Node n []) = show n
showSkew (Node n xs) = "(" <> intercalate " " strs <> ")"
 where
  strs = show n : (showSkew <$> reverse xs)

binLeaf :: Bin -> Tree Leaf
binLeaf = error "TODO flip order" {- go []
  where
   go a (N n)     = Node (T n) a
   go a (x :@ y)  = go (binLeaf y:a) x -}

leafBin :: Tree Leaf -> bin
leafBin = error "TODO"

run :: Bin -> Bin
run = leafBin . nf . binLeaf

arity :: Leaf -> Int
arity (J n _ _) = fromIntegral n
arity (T n) = case n of { 0→3; 1→2; 2→2; 3→3; 4→1; 5→4; _→1 }

pattern NT :: Nat -> Tree Leaf
pattern NT n = Node (T n) []

infixl 5 %;

(%) :: Tree Leaf -> Tree Leaf -> Tree Leaf
(%) (Node n xs) x = Node n (x:xs)

nf :: Tree Leaf -> Tree Leaf
nf (whnf -> Node h xs) = Node h (nf <$> xs)

whnf :: Tree Leaf -> Tree Leaf
whnf = go
 where
  go = \case
    Node (T 0) [z,y,x] -> go (x % z % (y % z))
    Node (T 1) [x,_]   -> go x
    Node (T 2) [x,y]   -> go x `seq` go y

    Node (T 3) [b,t,n] -> case (whnf n, whnf t) of
      ( NT 0,  NT _  ) -> nf b
      ( NT nv, NT tv ) -> Node (J nv tv (nf b)) []
      ( _,     NT _  ) -> error "arity not atom"
      ( _,     _     ) -> error "tag not atom"

    Node (T 4) [x] -> case (whnf x) of
      NT n -> NT (n+1)
      _    -> error "inc not atom"

    Node (T 5) [x,z,a,c] -> case whnf x of
      Node (T 0) []     -> go z
      Node (T n) []     -> go (a % NT(n-1))
      Node (J n t b) [] -> go (c % (NT 3 % NT n % NT t) % b)
      Node n (v:vs)     -> go (c % (Node n vs) % v)

    Node (J n _ (Node b bs)) xs | length xs == fromIntegral n ->
      whnf (Node b (xs <> bs))

    Node (T n) _ | n>5 -> error ("bad opcode: " <> show n)

    Node x []                       -> Node x []
    Node x xs | length xs < arity x -> Node x xs
    Node n (x:xs)                   -> go (go (Node n xs) % x)
