{-# OPTIONS_GHC -Wall #-}

module Urbit.Moon.Prim
  ( Op(..)
  ) where

import Prelude ()
import ClassyPrelude
import GHC.Natural

import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)

-- Types -----------------------------------------------------------------------

type Nat = Natural

data Op a
  = NAT_LIT Natural
  | NAT_INT a
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

  | INT_LIT Integer
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

  | VEC_LIT [a]
  | VEC_GEN a a
  | VEC_IDX a a
  | VEC_SET a a a
  | VEC_UPD a a a
  | VEC_LIS a
  | VEC_MAP a a
  | VEC_CAT a a

  | WOR_BIT_NOT  Nat a
  | WOR_BIT_OR   Nat a a
  | WOR_BIT_AND  Nat a a
  | WOR_BIT_NAND Nat a a
  | WOR_BIT_NOR  Nat a a
  | WOR_BIT_XOR  Nat a a
  | WOR_BIT_XNOR Nat a a

  | WOR_NAT_LIT Nat Nat
  | WOR_NAT_MAK Nat a a
  | WOR_NAT_NAT Nat a
  | WOR_NAT_ZER Nat a
  | WOR_NAT_INC Nat a
  | WOR_NAT_DEC Nat a
  | WOR_NAT_SUB Nat a a
  | WOR_NAT_MUL Nat a a

  | WOR_INT_LIT Nat Integer
  | WOR_INT_MAK Nat a a
  | WOR_INT_INT Nat a
  | WOR_INT_ZER Nat a
  | WOR_INT_NEG Nat a
  | WOR_INT_INC Nat a
  | WOR_INT_DEC Nat a
  | WOR_INT_SUB Nat a a
  | WOR_INT_MUL Nat a a

  | BUF_STR Text
  | BUF_LIT Nat [Natural]
  | BUF_GEN Nat a a
  | BUF_IDX Nat a a
  | BUF_SET Nat a a a
  | BUF_UPD Nat a a a
  | BUF_LIS Nat a
  | BUF_MAP Nat a a
  | BUF_ITR Nat a a
  | BUF_CAT Nat a a
 deriving (Eq, Ord, Functor, Foldable, Traversable)

-- Instances -------------------------------------------------------------------

deriveEq1   ''Op
deriveOrd1  ''Op
deriveRead1 ''Op
deriveShow1 ''Op

instance Num (Op a) where
    fromInteger = NAT_LIT . fromIntegral
    (+)    = error "Fake Num instance for `Exp`"
    (-)    = error "Fake Num instance for `Exp`"
    (*)    = error "Fake Num instance for `Exp`"
    abs    = error "Fake Num instance for `Exp`"
    signum = error "Fake Num instance for `Exp`"
    negate = error "Fake Num instance for `Exp`"

instance Show a => Show (Op a) where
  show = \case
    NAT_LIT n      -> show n
    NAT_INT x      -> prim "NAT_INT" [x]
    NAT_RUN x z li -> prim "NAT_RUN" [x,z,li]
    NAT_ZER x      -> prim "NAT_ZER" [x]
    NAT_INC x      -> prim "NAT_INC" [x]
    NAT_DEC x      -> prim "NAT_DEC" [x]
    NAT_EQL x y    -> prim "NAT_EQL" [x,y]
    NAT_MUL x y    -> prim "NAT_MUL" [x,y]
    NAT_DIV x y    -> prim "NAT_DIV" [x,y]
    NAT_ADD x y    -> prim "NAT_ADD" [x,y]
    NAT_SUB x y    -> prim "NAT_SUB" [x,y]
    NAT_MOD x y    -> prim "NAT_MOD" [x,y]

    INT_LIT i   -> showIntLit i
    INT_NAT x   -> prim "INT_NAT" [x]
    INT_ZER x   -> prim "INT_ZER" [x]
    INT_INC x   -> prim "INT_INC" [x]
    INT_DEC x   -> prim "INT_DEC" [x]
    INT_NEG x   -> prim "INT_NEG" [x]
    INT_EQL x y -> prim "INT_EQL" [x,y]
    INT_MUL x y -> prim "INT_MUL" [x,y]
    INT_DIV x y -> prim "INT_DIV" [x,y]
    INT_ADD x y -> prim "INT_ADD" [x,y]
    INT_SUB x y -> prim "INT_SUB" [x,y]
    INT_MOD x y -> prim "INT_MOD" [x,y]

    VEC_LIT xs    -> "#" <> brak xs
    VEC_GEN x l   -> prim "VEC_GEN" [x,l]
    VEC_IDX x i   -> prim "VEC_IDX" [x,i]
    VEC_SET x i v -> prim "VEC_SET" [x,i,v]
    VEC_UPD x i f -> prim "VEC_UPD" [x,i,f]
    VEC_LIS x     -> prim "VEC_LIS" [x]
    VEC_MAP x l   -> prim "VEC_MAP" [x,l]
    VEC_CAT x y   -> prim "VEC_CAT" [x,y]

    WOR_BIT_NOT  n x   -> sizPrim "WOR_BIT_NOT"  n [x]
    WOR_BIT_OR   n x y -> sizPrim "WOR_BIT_OR"   n [x,y]
    WOR_BIT_AND  n x y -> sizPrim "WOR_BIT_AND"  n [x,y]
    WOR_BIT_NAND n x y -> sizPrim "WOR_BIT_NAND" n [x,y]
    WOR_BIT_NOR  n x y -> sizPrim "WOR_BIT_NOR"  n [x,y]
    WOR_BIT_XOR  n x y -> sizPrim "WOR_BIT_XOR"  n [x,y]
    WOR_BIT_XNOR n x y -> sizPrim "WOR_BIT_XNOR" n [x,y]

    WOR_NAT_LIT n v   -> show v <> "w" <> show (siz n)
    WOR_NAT_MAK n x y -> sizPrim "WOR_NAT_MAK" n [x,y]
    WOR_NAT_NAT n x   -> sizPrim "WOR_NAT_NAT" n [x]
    WOR_NAT_ZER n x   -> sizPrim "WOR_NAT_ZER" n [x]
    WOR_NAT_INC n x   -> sizPrim "WOR_NAT_INC" n [x]
    WOR_NAT_DEC n x   -> sizPrim "WOR_NAT_DEC" n [x]
    WOR_NAT_SUB n x y -> sizPrim "WOR_NAT_SUB" n [x,y]
    WOR_NAT_MUL n x y -> sizPrim "WOR_NAT_MUL" n [x,y]

    WOR_INT_LIT n v   -> showIntLit v <> "w" <> show (siz n)
    WOR_INT_MAK n x y -> sizPrim "WOR_INT_MAK" n [x,y]
    WOR_INT_INT n x   -> sizPrim "WOR_INT_INT" n [x]
    WOR_INT_ZER n x   -> sizPrim "WOR_INT_ZER" n [x]
    WOR_INT_NEG n x   -> sizPrim "WOR_INT_NEG" n [x]
    WOR_INT_INC n x   -> sizPrim "WOR_INT_INC" n [x]
    WOR_INT_DEC n x   -> sizPrim "WOR_INT_DEC" n [x]
    WOR_INT_SUB n x y -> sizPrim "WOR_INT_SUB" n [x,y]
    WOR_INT_MUL n x y -> sizPrim "WOR_INT_MUL" n [x,y]

    BUF_STR s       -> "\"" <> unpack s <> "\""
    BUF_LIT n vs    -> "#w" <> show (siz n) <> brakStr (show <$> vs)
    BUF_GEN n x l   -> sizPrim "BUF_GEN" n [x,l]
    BUF_IDX n x i   -> sizPrim "BUF_IDX" n [x,i]
    BUF_SET n x i v -> sizPrim "BUF_SET" n [x,i,v]
    BUF_UPD n x i l -> sizPrim "BUF_UPD" n [x,i,l]
    BUF_LIS n x     -> sizPrim "BUF_LIS" n [x]
    BUF_MAP n x l   -> sizPrim "BUF_MAP" n [x,l]
    BUF_ITR n x l   -> sizPrim "BUF_ITR" n [x,l]
    BUF_CAT n x y   -> sizPrim "BUF_CAT" n [x,y]


-- Helpers ---------------------------------------------------------------------

showIntLit :: Integer -> String
showIntLit i | i >= 0 = "+" <> show i
showIntLit i          = show i

siz :: Nat -> Nat
siz 0 = 0
siz n = 2^(n-1)

brakStr :: [String] -> String
brakStr xs = "[" <> intercalate " " xs <> "]"

brak :: Show a => [a] -> String
brak xs = brakStr (show <$> xs)

sizPrim :: Show a => String -> Nat -> [a] -> String
sizPrim nm n xs = prim (nm <> "_W" <> show (siz n)) xs

prim :: Show a => String -> [a] -> String
prim nm [] = nm
prim nm xs = "(" <> nm <> " " <> intercalate " " (show <$> xs) <> ")"
