{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Moon.Prim
  ( Op(..)
  ) where

import ClassyPrelude

import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Urbit.Runic   (Runic(..), ToRunic(..), appRune, runicShowWide)


-- Types -----------------------------------------------------------------------

data Op a
  = NAT_INT a
  | NAT_RUN a a a
  | NAT_ZER a
  | NAT_INC a
  | NAT_DEC a
  | NAT_EQL a a
  | NAT_MUL a a
  | NAT_DIV a a
  | NAT_ADD a a
  | NAT_SUB a a
  | NAT_MOD a a

  | INT_NAT a
  | INT_ZER a
  | INT_INC a
  | INT_DEC a
  | INT_NEG a
  | INT_EQL a a
  | INT_MUL a a
  | INT_DIV a a
  | INT_ADD a a
  | INT_SUB a a
  | INT_MOD a a

  | VEC_FROM_LIST a
  | VEC_GEN a a
  | VEC_IDX a a
  | VEC_SET a a a
  | VEC_UPD a a a
  | VEC_LIS a
  | VEC_MAP a a
  | VEC_CAT a a

  | WOR_NOT  a a
  | WOR_OR   a a a
  | WOR_AND  a a a
  | WOR_NAND a a a
  | WOR_NOR  a a a
  | WOR_XOR  a a a
  | WOR_XNOR a a a

  | WOR_NAT_FROM_NAT a a a
  | WOR_NAT_TO_NAT a a
  | WOR_NAT_ZER a a
  | WOR_NAT_INC a a
  | WOR_NAT_DEC a a
  | WOR_NAT_SUB a a a
  | WOR_NAT_MUL a a a

  | WOR_INT_MAK a a a
  | WOR_INT_INT a a
  | WOR_INT_ZER a a
  | WOR_INT_NEG a a
  | WOR_INT_INC a a
  | WOR_INT_DEC a a
  | WOR_INT_SUB a a a
  | WOR_INT_MUL a a a

  | BUF_GEN a a a
  | BUF_IDX a a a
  | BUF_SET a a a a
  | BUF_UPD a a a a
  | BUF_LIS a a
  | BUF_MAP a a a
  | BUF_ITR a a a
  | BUF_CAT a a a
 deriving (Eq, Ord, Functor, Foldable, Traversable)


-- Instances -------------------------------------------------------------------

deriveEq1   ''Op
deriveOrd1  ''Op
deriveRead1 ''Op
deriveShow1 ''Op

instance (ToRunic a, Show a) => Show (Op a) where
  show = unpack . runicShowWide

primRune :: ToRunic a => Text -> [a] -> Runic
primRune t xs = appRune (Leaf t : fmap toRunic xs)

--  TODO Use generic deriving?
instance ToRunic a => ToRunic (Op a) where
  toRunic = \case
    NAT_INT x              -> primRune "NAT_INT" [x]
    NAT_RUN x z li         -> primRune "NAT_RUN" [x,z,li]
    NAT_ZER x              -> primRune "NAT_ZER" [x]
    NAT_INC x              -> primRune "NAT_INC" [x]
    NAT_DEC x              -> primRune "NAT_DEC" [x]
    NAT_EQL x y            -> primRune "NAT_EQL" [x,y]
    NAT_MUL x y            -> primRune "NAT_MUL" [x,y]
    NAT_DIV x y            -> primRune "NAT_DIV" [x,y]
    NAT_ADD x y            -> primRune "NAT_ADD" [x,y]
    NAT_SUB x y            -> primRune "NAT_SUB" [x,y]
    NAT_MOD x y            -> primRune "NAT_MOD" [x,y]
    INT_NAT x              -> primRune "INT_NAT" [x]
    INT_ZER x              -> primRune "INT_ZER" [x]
    INT_INC x              -> primRune "INT_INC" [x]
    INT_DEC x              -> primRune "INT_DEC" [x]
    INT_NEG x              -> primRune "INT_NEG" [x]
    INT_EQL x y            -> primRune "INT_EQL" [x,y]
    INT_MUL x y            -> primRune "INT_MUL" [x,y]
    INT_DIV x y            -> primRune "INT_DIV" [x,y]
    INT_ADD x y            -> primRune "INT_ADD" [x,y]
    INT_SUB x y            -> primRune "INT_SUB" [x,y]
    INT_MOD x y            -> primRune "INT_MOD" [x,y]
    VEC_FROM_LIST x        -> primRune "VEC_FROM_LIST" [x]
    VEC_GEN x l            -> primRune "VEC_GEN" [x,l]
    VEC_IDX x i            -> primRune "VEC_IDX" [x,i]
    VEC_SET x i v          -> primRune "VEC_SET" [x,i,v]
    VEC_UPD x i f          -> primRune "VEC_UPD" [x,i,f]
    VEC_LIS x              -> primRune "VEC_LIS" [x]
    VEC_MAP x l            -> primRune "VEC_MAP" [x,l]
    VEC_CAT x y            -> primRune "VEC_CAT" [x,y]
    WOR_NOT  n x           -> primRune "WOR_NOT"  [n,x]
    WOR_OR   n x y         -> primRune "WOR_OR"   [n,x,y]
    WOR_AND  n x y         -> primRune "WOR_AND"  [n,x,y]
    WOR_NAND n x y         -> primRune "WOR_NAND" [n,x,y]
    WOR_NOR  n x y         -> primRune "WOR_NOR"  [n,x,y]
    WOR_XOR  n x y         -> primRune "WOR_XOR"  [n,x,y]
    WOR_XNOR n x y         -> primRune "WOR_XNOR" [n,x,y]
    WOR_NAT_FROM_NAT n x y -> primRune "WOR_NAT_FROM_NAT" [n,x,y]
    WOR_NAT_TO_NAT n x     -> primRune "WOR_NAT_TO_NAT" [n,x]
    WOR_NAT_ZER n x        -> primRune "WOR_NAT_ZER" [n,x]
    WOR_NAT_INC n x        -> primRune "WOR_NAT_INC" [n,x]
    WOR_NAT_DEC n x        -> primRune "WOR_NAT_DEC" [n,x]
    WOR_NAT_SUB n x y      -> primRune "WOR_NAT_SUB" [n,x,y]
    WOR_NAT_MUL n x y      -> primRune "WOR_NAT_MUL" [n,x,y]
    WOR_INT_MAK n x y      -> primRune "WOR_INT_MAK" [n,x,y]
    WOR_INT_INT n x        -> primRune "WOR_INT_INT" [n,x]
    WOR_INT_ZER n x        -> primRune "WOR_INT_ZER" [n,x]
    WOR_INT_NEG n x        -> primRune "WOR_INT_NEG" [n,x]
    WOR_INT_INC n x        -> primRune "WOR_INT_INC" [n,x]
    WOR_INT_DEC n x        -> primRune "WOR_INT_DEC" [n,x]
    WOR_INT_SUB n x y      -> primRune "WOR_INT_SUB" [n,x,y]
    WOR_INT_MUL n x y      -> primRune "WOR_INT_MUL" [n,x,y]
    BUF_GEN n x l          -> primRune "BUF_GEN" [n,x,l]
    BUF_IDX n x i          -> primRune "BUF_IDX" [n,x,i]
    BUF_SET n x i v        -> primRune "BUF_SET" [n,x,i,v]
    BUF_UPD n x i l        -> primRune "BUF_UPD" [n,x,i,l]
    BUF_LIS n x            -> primRune "BUF_LIS" [n,x]
    BUF_MAP n x l          -> primRune "BUF_MAP" [n,x,l]
    BUF_ITR n x l          -> primRune "BUF_ITR" [n,x,l]
    BUF_CAT n x y          -> primRune "BUF_CAT" [n,x,y]
