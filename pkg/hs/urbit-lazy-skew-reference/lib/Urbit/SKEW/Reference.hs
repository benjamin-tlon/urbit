module Urbit.SKEW.Reference
  ( Expr(..)
  , SKEW(..)
  , nf
  , whnf
  , eval
  )
where

import Control.Monad   (guard)
import Data.Foldable   (foldl')
import Data.List       (intercalate)
import Data.Tree       (Tree(Node))
import Numeric.Natural (Natural)

infixl 5 :@;

data SKEW = S | K | E | W | Q | J Natural Expr Expr
  deriving (Eq, Ord, Show)

data Expr = N SKEW | Expr :@ Expr
  deriving (Eq, Ord)

tree :: Expr -> Tree SKEW
tree = go [] where go a = \case { N n -> Node n a; x :@ y -> go (tree y:a) x }

expr :: Tree SKEW -> Expr
expr (Node n xs) = foldl' (:@) (N n) (expr <$> xs)

showTree :: Tree SKEW -> String
showTree (Node n []) = show n
showTree (Node n xs) = "(" <> intercalate " " (show n : fmap showTree xs) <> ")"

instance Show Expr where show = showTree . tree

eval :: Expr -> Expr
eval = \case { (nf -> Just x) -> eval x; x -> x }

nf :: Expr -> Maybe Expr
nf = \case
  (whnf -> Just x)    -> Just x
  (nf -> Just x) :@ y -> Just (x :@ y)
  x :@ (nf -> Just y) -> Just (x :@ y)
  _                   -> Nothing

whnf :: Expr -> Maybe Expr
whnf = \case
  N S :@ x :@ y :@ z                        -> Just (x :@ z :@ (y :@ z))
  N K :@ x :@ y                             -> Just x
  N E :@ (nf -> Just x) :@ y                -> Just (N E :@ x :@ y)
  N E :@ x :@ (nf -> Just y)                -> Just (N E :@ x :@ y)
  N E :@ x :@ y                             -> Just (N (J 1 x y))
  N (J n (N E) t) :@ (nf -> Just b)         -> Just (N (J n (N E) t) :@ b)
  N (J n (N E) t) :@ b                      -> Just (N (J (n+1) t b))
  N W :@ a:@s:@k:@e:@w:@q :@ (whnf->Just x) -> Just (N W:@a:@s:@k:@e:@w:@q:@x)
  N W :@ a:@s:@k:@e:@w:@q :@ (x:@y)         -> Just (a :@ x :@ y)
  N W :@ a:@s:@k:@e:@w:@q :@ N S            -> Just s
  N W :@ a:@s:@k:@e:@w:@q :@ N K            -> Just k
  N W :@ a:@s:@k:@e:@w:@q :@ N E            -> Just e
  N W :@ a:@s:@k:@e:@w:@q :@ N W            -> Just w
  N W :@ a:@s:@k:@e:@w:@q :@ N Q            -> Just q
  N W :@ a:@s:@k:@e:@w:@q :@ N (J 1 t b)    -> Just (a :@ (N E :@ t) :@ b)
  N W :@ a:@s:@k:@e:@w:@q :@ N (J n t b)    -> Just (a :@ jn (n-1) (N E) t :@ b)
  N Q :@ (whnf -> Just x) :@ y              -> Just (N Q :@ x :@ y)
  N Q :@ x :@ y                             -> Just y
  (tree -> Node (J n t b) xs) | len xs == n -> Just (foldl' (:@) b (expr<$>xs))
  (whnf -> Just xv) :@ y                    -> Just (xv :@ y)
  _                                         -> Nothing
 where
  len :: [a] -> Natural
  len = fromIntegral . length

  jn :: Natural -> Expr -> Expr -> Expr
  jn n t b = N (J n t b)
