module Urbit.Moon.Exp where

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

  | NatLIT Natural                  --  Nat
  | NatINT (Exp a)                  --  Nat → Int
  | NatRUN (Exp a) (Exp a) (Exp a)  --  Nat → a → (a→a) → a
  | NatZER (Exp a)                  --  Nat → Bol
  | NatINC (Exp a)                  --  Nat → Nat
  | NatDEC (Exp a)                  --  Nat → Nat
  | NatEQL (Exp a) (Exp a)          --  Nat → Nat → Bol
  | NatMUL (Exp a) (Exp a)          --  Nat → Nat → Nat
  | NatDIV (Exp a) (Exp a)          --  Nat → Nat → Nat
  | NatADD (Exp a) (Exp a)          --  Nat → Nat → Nat
  | NatSUB (Exp a) (Exp a)          --  Nat → Nat → Nat
  | NatMOD (Exp a) (Exp a)          --  Nat → Nat → Nat

  | IntLIT Integer          --  Int
  | IntNAT (Exp a)          --  Int → Nat
  | IntZER (Exp a)          --  Int → Bol
  | IntINC (Exp a)          --  Int → Int
  | IntDEC (Exp a)          --  Int → Int
  | IntNEG (Exp a)          --  Int → Int
  | IntEQL (Exp a) (Exp a)  --  Int → Int → Bol
  | IntMUL (Exp a) (Exp a)  --  Int → Int → Int
  | IntDIV (Exp a) (Exp a)  --  Int → Int → Int
  | IntADD (Exp a) (Exp a)  --  Int → Int → Int
  | IntSUB (Exp a) (Exp a)  --  Int → Int → Int
  | IntMOD (Exp a) (Exp a)  --  Int → Int → Int

  | VecLIT [Exp a]                  --  Vec a
  | VecGEN (Exp a) (Exp a)          --  (n:Nat) → (Nat→a) → Vec a
  | VecIDX (Exp a) (Exp a)          --  Vec a → Nat → Opt a
  | VecSET (Exp a) (Exp a) (Exp a)  --  Vec a → Nat → a → Vec a
  | VecUPD (Exp a) (Exp a) (Exp a)  --  Vec a → Nat → (a→a) → Vec a
  | VecLIS (Exp a)                  --  Vec a → [a]
  | VecMAP (Exp a) (Exp a)          --  Vec a → (a→b) → Vec b
  | VecCAT (Exp a) (Exp a)          --  Vec a → Vec a → Vec a

  | WorBitNOT  Nat (Exp a)          --  Wn → Wn
  | WorBitOR   Nat (Exp a) (Exp a)  --  Wn → Wn → Wn
  | WorBitAND  Nat (Exp a) (Exp a)  --  Wn → Wn → Wn
  | WorBitNAND Nat (Exp a) (Exp a)  --  Wn → Wn → Wn
  | WorBitNOR  Nat (Exp a) (Exp a)  --  Wn → Wn → Wn
  | WorBitXOR  Nat (Exp a) (Exp a)  --  Wn → Wn → Wn
  | WorBitXNOR Nat (Exp a) (Exp a)  --  Wn → Wn → Wn

  | WorNatLIT Nat Nat              --  Wn
  | WorNatMAK Nat (Exp a) (Exp a)  --  Nat → Wn
  | WorNatNAT Nat (Exp a)          --  Wn → Nat
  | WorNatZER Nat (Exp a)          --  Wn → Bol
  | WorNatINC Nat (Exp a)          --  Wn → Wn
  | WorNatDEC Nat (Exp a)          --  Wn → Wn
  | WorNatSUB Nat (Exp a) (Exp a)  --  Wn → Wn → Wn
  | WorNatMUL Nat (Exp a) (Exp a)  --  Wn → Wn → Wn

  | WorIntLIT Nat Integer          --  Wn
  | WorIntMAK Nat (Exp a) (Exp a)  --  (n:Nat) → Int → Wn
  | WorIntINT Nat (Exp a)          --  Wn → Int
  | WorIntZER Nat (Exp a)          --  Wn → Bol
  | WorIntNEG Nat (Exp a)          --  Wn → Wn
  | WorIntINC Nat (Exp a)          --  Wn → Wn
  | WorIntDEC Nat (Exp a)          --  Wn → Wn
  | WorIntSUB Nat (Exp a) (Exp a)  --  Wn → Wn → Wn
  | WorIntMUL Nat (Exp a) (Exp a)  --  Wn → Wn → Wn

  | BufLIT Nat [Natural]                --  Bn
  | BufSTR     Text                     --  B8
  | BufGEN Nat (Exp a) (Exp a)          --  Nat → (Nat→Wn) → Bn
  | BufIDX Nat (Exp a) (Exp a)          --  Bn → Nat → Opt a
  | BufSET Nat (Exp a) (Exp a) (Exp a)  --  Bn → Nat → Wn → Bn
  | BufUPD Nat (Exp a) (Exp a) (Exp a)  --  Bn → Nat → (Wn→Wn) → Bn
  | BufLIS Nat (Exp a)                  --  Bn → [Wn]
  | BufMAP Nat (Exp a) (Exp a)          --  Bn → (Wn→Wn) → Bn
  | BufITR Nat (Exp a) (Exp a)          --  Bn → (Nat→Wn→Wn) → Bn
  | BufCAT Nat (Exp a) (Exp a)          --  Bn → Bn → Bn
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

  NatLIT n     >>= f = NatLIT n
  NatINT x     >>= f = NatINT (x >>= f)
  NatRUN x z i >>= f = NatRUN (x >>= f) (z >>= f) (i >>= f)
  NatZER x     >>= f = NatZER (x >>= f)
  NatINC x     >>= f = NatINC (x >>= f)
  NatDEC x     >>= f = NatDEC (x >>= f)
  NatEQL x y   >>= f = NatEQL (x >>= f) (y >>= f)
  NatMUL x y   >>= f = NatMUL (x >>= f) (y >>= f)
  NatDIV x y   >>= f = NatDIV (x >>= f) (y >>= f)
  NatADD x y   >>= f = NatADD (x >>= f) (y >>= f)
  NatSUB x y   >>= f = NatSUB (x >>= f) (y >>= f)
  NatMOD x y   >>= f = NatMOD (x >>= f) (y >>= f)

  IntLIT n       >>= f = IntLIT n
  IntNAT x       >>= f = IntNAT (x >>= f)
  IntZER x       >>= f = IntZER (x >>= f)
  IntINC x       >>= f = IntINC (x >>= f)
  IntDEC x       >>= f = IntDEC (x >>= f)
  IntNEG x       >>= f = IntNEG (x >>= f)
  IntEQL x y     >>= f = IntEQL (x >>= f) (y >>= f)
  IntMUL x y     >>= f = IntMUL (x >>= f) (y >>= f)
  IntDIV x y     >>= f = IntDIV (x >>= f) (y >>= f)
  IntADD x y     >>= f = IntADD (x >>= f) (y >>= f)
  IntSUB x y     >>= f = IntSUB (x >>= f) (y >>= f)
  IntMOD x y     >>= f = IntMOD (x >>= f) (y >>= f)

  VecLIT xs    >>= f = VecLIT (map (>>= f) xs)
  VecGEN x b   >>= f = VecGEN (x >>= f) (b >>= f)
  VecIDX x y   >>= f = VecIDX (x >>= f) (y >>= f)
  VecSET x y z >>= f = VecSET (x >>= f) (y >>= f) (z >>= f)
  VecUPD x y b >>= f = VecUPD (x >>= f) (y >>= f) (b >>= f)
  VecLIS x     >>= f = VecLIS (x >>= f)
  VecMAP x b   >>= f = VecMAP (x >>= f) (b >>= f)
  VecCAT x y   >>= f = VecCAT (x >>= f) (y >>= f)

  WorBitNOT  n x   >>= f = WorBitNOT  n (x >>= f)
  WorBitOR   n x y >>= f = WorBitOR   n (x >>= f) (y >>= f)
  WorBitAND  n x y >>= f = WorBitAND  n (x >>= f) (y >>= f)
  WorBitNAND n x y >>= f = WorBitNAND n (x >>= f) (y >>= f)
  WorBitNOR  n x y >>= f = WorBitNOR  n (x >>= f) (y >>= f)
  WorBitXOR  n x y >>= f = WorBitXOR  n (x >>= f) (y >>= f)
  WorBitXNOR n x y >>= f = WorBitXNOR n (x >>= f) (y >>= f)

  WorNatLIT n v   >>= f = WorNatLIT n v
  WorNatMAK n x y >>= f = WorNatMAK n (x >>= f) (y >>= f)
  WorNatNAT n x   >>= f = WorNatNAT n (x >>= f)
  WorNatZER n x   >>= f = WorNatZER n (x >>= f)
  WorNatINC n x   >>= f = WorNatINC n (x >>= f)
  WorNatDEC n x   >>= f = WorNatDEC n (x >>= f)
  WorNatSUB n x y >>= f = WorNatSUB n (x >>= f) (y >>= f)
  WorNatMUL n x y >>= f = WorNatMUL n (x >>= f) (y >>= f)

  WorIntLIT n v   >>= f = WorIntLIT n v
  WorIntMAK n x y >>= f = WorIntMAK n (x >>= f) (y >>= f)
  WorIntINT n x   >>= f = WorIntINT n (x >>= f)
  WorIntZER n x   >>= f = WorIntZER n (x >>= f)
  WorIntNEG n x   >>= f = WorIntNEG n (x >>= f)
  WorIntINC n x   >>= f = WorIntINC n (x >>= f)
  WorIntDEC n x   >>= f = WorIntDEC n (x >>= f)
  WorIntSUB n x y >>= f = WorIntSUB n (x >>= f) (y >>= f)
  WorIntMUL n x y >>= f = WorIntMUL n (x >>= f) (y >>= f)

  BufLIT n vs    >>= f = BufLIT n vs
  BufSTR   tx    >>= f = BufSTR   tx
  BufGEN n x b   >>= f = BufGEN n (x >>= f) (b >>= f)
  BufIDX n x i   >>= f = BufIDX n (x >>= f) (i >>= f)
  BufSET n x i v >>= f = BufSET n (x >>= f) (i >>= f) (v >>= f)
  BufUPD n x i b >>= f = BufUPD n (x >>= f) (i >>= f) (b >>= f)
  BufLIS n x     >>= f = BufLIS n (x >>= f)
  BufMAP n x b   >>= f = BufMAP n (x >>= f) (b >>= f)
  BufITR n x b   >>= f = BufITR n (x >>= f) (b >>= f)
  BufCAT n x y   >>= f = BufCAT n (x >>= f) (y >>= f)
