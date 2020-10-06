module Urbit.Moon.AST where

import Prelude ()
import Bound
import ClassyPrelude
import GHC.Natural
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Functor.Classes (showsPrec1)


-- Types -----------------------------------------------------------------------

type Nat = Natural

type ADTShape = [Nat]

data Bind = Bind Text AST
  deriving (Eq, Ord)

data Pat = MkPat { patBinds :: [Text], patExpr :: AST }
 deriving (Eq, Ord)

data Con a = MkCon [Nat] [a] [Nat]
 deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data AST
  = VAR Text
  | APP AST AST
  | LAM Bind
  | LET [Bind] AST
  | SEQ AST AST
  | CON (Con AST)
  | PAT AST [Pat]

  | NAT_LIT Natural
  | NAT_INT AST
  | NAT_RUN AST AST AST
  | NAT_ZER AST
  | NAT_INC AST
  | NAT_DEC AST
  | NAT_EQL AST AST
  | NAT_MUL AST AST
  | NAT_DIV AST AST
  | NAT_ADD AST AST
  | NAT_SUB AST AST
  | NAT_MOD AST AST

  | INT_LIT Integer
  | INT_NAT AST
  | INT_ZER AST
  | INT_INC AST
  | INT_DEC AST
  | INT_NEG AST
  | INT_EQL AST AST
  | INT_MUL AST AST
  | INT_DIV AST AST
  | INT_ADD AST AST
  | INT_SUB AST AST
  | INT_MOD AST AST

  | VEC_LIT [AST]
  | VEC_GEN AST AST
  | VEC_IDX AST AST
  | VEC_SET AST AST AST
  | VEC_UPD AST AST AST
  | VEC_LIS AST
  | VEC_MAP AST AST
  | VEC_CAT AST AST

  | WOR_BIT_NOT  Nat AST
  | WOR_BIT_OR   Nat AST AST
  | WOR_BIT_AND  Nat AST AST
  | WOR_BIT_NAND Nat AST AST
  | WOR_BIT_NOR  Nat AST AST
  | WOR_BIT_XOR  Nat AST AST
  | WOR_BIT_XNOR Nat AST AST

  | WOR_NAT_LIT Nat Nat
  | WOR_NAT_MAK Nat AST AST
  | WOR_NAT_NAT Nat AST
  | WOR_NAT_ZER Nat AST
  | WOR_NAT_INC Nat AST
  | WOR_NAT_DEC Nat AST
  | WOR_NAT_SUB Nat AST AST
  | WOR_NAT_MUL Nat AST AST

  | WOR_INT_LIT Nat Integer
  | WOR_INT_MAK Nat AST AST
  | WOR_INT_INT Nat AST
  | WOR_INT_ZER Nat AST
  | WOR_INT_NEG Nat AST
  | WOR_INT_INC Nat AST
  | WOR_INT_DEC Nat AST
  | WOR_INT_SUB Nat AST AST
  | WOR_INT_MUL Nat AST AST

  | BUF_STR Text
  | BUF_LIT Nat [Natural]
  | BUF_GEN Nat AST AST
  | BUF_IDX Nat AST AST
  | BUF_SET Nat AST AST AST
  | BUF_UPD Nat AST AST AST
  | BUF_LIS Nat AST
  | BUF_MAP Nat AST AST
  | BUF_ITR Nat AST AST
  | BUF_CAT Nat AST AST
 deriving (Eq, Ord)


-- Instances -------------------------------------------------------------------

instance IsString AST where
    fromString = VAR . pack

instance Num AST where
    fromInteger = NAT_LIT . fromIntegral
    (+)    = error "Fake Num instance for `Exp`"
    (-)    = error "Fake Num instance for `Exp`"
    (*)    = error "Fake Num instance for `Exp`"
    abs    = error "Fake Num instance for `Exp`"
    signum = error "Fake Num instance for `Exp`"
    negate = error "Fake Num instance for `Exp`"

showLet :: [Bind] -> AST -> String
showLet bs x = "/=(" <> go bs <> show x <> ")"
 where
  go = \case
    []             -> ""
    Bind v vb : bs -> unpack v <> " " <> show vb <> ", " <> go bs

instance Show AST where
  show = \case
    VAR t          -> unpack t
    APP f x        -> "(" <> show f <> " " <> show x <> ")"
    LAM (Bind v b) -> mconcat ["|=(", unpack v <> " ", show b <> ")"]
    LET bs x       -> showLet bs x
    SEQ x y        -> mconcat ["!!(", show x, " ", show y, ")"]
    CON con        -> showCon con
    PAT x pats     -> showPat x pats

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

    INT_LIT i | i>=0 -> "+" <> show i
    INT_LIT i        -> show i
    INT_NAT x        -> prim "INT_NAT" [x]
    INT_ZER x        -> prim "INT_ZER" [x]
    INT_INC x        -> prim "INT_INC" [x]
    INT_DEC x        -> prim "INT_DEC" [x]
    INT_NEG x        -> prim "INT_NEG" [x]
    INT_EQL x y      -> prim "INT_EQL" [x,y]
    INT_MUL x y      -> prim "INT_MUL" [x,y]
    INT_DIV x y      -> prim "INT_DIV" [x,y]
    INT_ADD x y      -> prim "INT_ADD" [x,y]
    INT_SUB x y      -> prim "INT_SUB" [x,y]
    INT_MOD x y      -> prim "INT_MOD" [x,y]

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

    WOR_INT_LIT n v   -> show (INT_LIT v) <> "w" <> show (siz n)
    WOR_INT_MAK n x y -> sizPrim "WOR_INT_MAK" n [x,y]
    WOR_INT_INT n x   -> sizPrim "WOR_INT_INT" n [x]
    WOR_INT_ZER n x   -> sizPrim "WOR_INT_ZER" n [x]
    WOR_INT_NEG n x   -> sizPrim "WOR_INT_NEG" n [x]
    WOR_INT_INC n x   -> sizPrim "WOR_INT_INC" n [x]
    WOR_INT_DEC n x   -> sizPrim "WOR_INT_DEC" n [x]
    WOR_INT_SUB n x y -> sizPrim "WOR_INT_SUB" n [x,y]
    WOR_INT_MUL n x y -> sizPrim "WOR_INT_MUL" n [x,y]

    BUF_STR s       -> "\"" <> unpack s <> "\""
    BUF_LIT n vs    -> "#w" <> show (siz n) <> brak (NAT_LIT <$> vs)
    BUF_GEN n x l   -> sizPrim "BUF_GEN" n [x,l]
    BUF_IDX n x i   -> sizPrim "BUF_IDX" n [x,i]
    BUF_SET n x i v -> sizPrim "BUF_SET" n [x,i,v]
    BUF_UPD n x i l -> sizPrim "BUF_UPD" n [x,i,l]
    BUF_LIS n x     -> sizPrim "BUF_LIS" n [x]
    BUF_MAP n x l   -> sizPrim "BUF_MAP" n [x,l]
    BUF_ITR n x l   -> sizPrim "BUF_ITR" n [x,l]
    BUF_CAT n x y   -> sizPrim "BUF_CAT" n [x,y]


-- Helpers ---------------------------------------------------------------------

siz :: Nat -> Nat
siz 0 = 0
siz n = 2^(n-1)

showPat :: AST -> [Pat] -> String
showPat x [MkPat [] y, MkPat [] n] =
  "?:(" <> show x <> " " <> show y <> " " <> show n <> ")"
showPat x pats =
  "?-(%" <> shapeName (patShape pats) <> " " <> show x <> go pats <> ")"
 where
  go []                   = ""
  go (MkPat vs body:more) = mconcat [ ", ", vars vs, " ", show body, go more ]

brak :: [AST] -> String
brak xs = "[" <> intercalate " " (show <$> xs) <> "]"

vars :: [Text] -> String
vars xs = "{" <> intercalate " " (unpack <$> xs) <> "}"

sizPrim :: String -> Nat -> [AST] -> String
sizPrim nm n xs = prim (nm <> "_W" <> show (siz n)) xs

prim :: String -> [AST] -> String
prim nm [] = nm
prim nm xs = "(" <> nm <> " " <> intercalate " " (show <$> xs) <> ")"


-- Generalized ADT Constructors ------------------------------------------------

conShape :: Con a -> ADTShape
conShape (MkCon l xs r) = l <> [fromIntegral (length xs)] <> r

patShape :: [Pat] -> ADTShape
patShape = map (fromIntegral . length . patBinds)

tup :: [AST] -> Con AST
tup xs = MkCon [] xs []

sig :: Con AST
sig = tup []

box :: AST -> Con AST
box x = tup [x]

con :: AST -> AST -> Con AST
con x y = tup [x,y]

tru, nah :: Con AST
tru = MkCon [0] [] []
nah = MkCon [] [] [0]

lef, rit :: AST -> Con AST
lef x = MkCon [] [x] [1]
rit x = MkCon [1] [x] []

opt_some :: AST -> Con AST
opt_some x = MkCon [0] [x] []

opt_none :: Con AST
opt_none = MkCon [] [] [1]

lis_null :: Con AST
lis_null = MkCon [] [] [2]

lis_cons :: AST -> AST -> Con AST
lis_cons x y = MkCon [0] [x,y] []

shapeName :: ADTShape -> String
shapeName []    = "VOID"
shapeName [0]   = "UNIT"
shapeName [1]   = "BOX"
shapeName [2]   = "PAIR"
shapeName [n]   = "TUPLE-" <> show n
shapeName [0,0] = "FLAG"
shapeName [1,1] = "EITHER"
shapeName [0,1] = "MAYBE"
shapeName [0,2] = "LIST"
shapeName ns = "ADT_" <> intercalate "_" (show <$> ns)

showCon :: Con AST -> String
showCon = \case
  MkCon []  []    []  -> brak []
  MkCon []  [x]   []  -> brak [x]
  MkCon []  [x,y] []  -> brak [x,y]
  MkCon []  xs    []  -> brak xs
  MkCon []  []    [0] -> prim "NAH" []
  MkCon [0] []    []  -> prim "YUP" []
  MkCon []  [x]   [1] -> prim "LEFT"  [x]
  MkCon [1] [x]   []  -> prim "RIGHT" [x]
  MkCon []  []    [1] -> prim "NONE" []
  MkCon [0] [x]   []  -> prim "SOME" [x]
  MkCon []  []    [2] -> prim "NULL" []
  MkCon [0] [x,y] []  -> prim "CONS" [x,y]

  c@(MkCon l xs r) -> prim name xs
   where
    name = shapeName (conShape c) <> "_CON_" <> (show . succ . length) l
