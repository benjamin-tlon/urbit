{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Moon.AST
  ( AST(..)
  , Pat(..)
  , Con(..)
  , Bind(..)
  ) where

import Prelude ()
import ClassyPrelude
import GHC.Natural
import Urbit.Moon.Prim (Op(..))


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
  | LAM Text AST
  | LET [Bind] AST
  | SEQ AST AST
  | CON (Con AST)
  | PAT AST [Pat]
  | OPR (Op AST)
 deriving (Eq, Ord)


-- Instances -------------------------------------------------------------------

instance IsString AST where
    fromString = VAR . pack

instance Num AST where
    fromInteger = OPR . NAT_LIT . fromIntegral
    (+)    = error "Fake Num instance for `Exp`"
    (-)    = error "Fake Num instance for `Exp`"
    (*)    = error "Fake Num instance for `Exp`"
    abs    = error "Fake Num instance for `Exp`"
    signum = error "Fake Num instance for `Exp`"
    negate = error "Fake Num instance for `Exp`"

showLet :: [Bind] -> AST -> String
showLet binds x = "/=(" <> go binds <> show x <> ")"
 where
  go = \case
    []             -> ""
    Bind v vb : bs -> unpack v <> " " <> show vb <> ", " <> go bs

instance Show AST where
  show = \case
    VAR t    -> unpack t
    APP f x  -> "(" <> show f <> " " <> show x <> ")"
    LAM v b  -> mconcat ["|=(", unpack v <> " ", show b <> ")"]
    LET bs x -> showLet bs x
    SEQ x y  -> mconcat ["!!(", show x, " ", show y, ")"]
    CON c    -> showCon c
    PAT x ps -> showPat x ps
    OPR oper -> show oper


-- Helpers ---------------------------------------------------------------------

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

_sig :: Con AST
_sig = tup []

_box :: AST -> Con AST
_box x = tup [x]

_con :: AST -> AST -> Con AST
_con x y = tup [x,y]

_tru, _nah :: Con AST
_tru = MkCon [0] [] []
_nah = MkCon [] [] [0]

_lef, _rit :: AST -> Con AST
_lef x = MkCon [] [x] [1]
_rit x = MkCon [1] [x] []

_opt_some :: AST -> Con AST
_opt_some x = MkCon [0] [x] []

_opt_none :: Con AST
_opt_none = MkCon [] [] [1]

_lis_null :: Con AST
_lis_null = MkCon [] [] [2]

_lis_cons :: AST -> AST -> Con AST
_lis_cons x y = MkCon [0] [x,y] []

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
  MkCon []  []    [0] -> prim "FALSE" []
  MkCon [0] []    []  -> prim "TRUE" []
  MkCon []  [x]   [1] -> prim "LEFT"  [x]
  MkCon [1] [x]   []  -> prim "RIGHT" [x]
  MkCon []  []    [1] -> prim "NONE" []
  MkCon [0] [x]   []  -> prim "SOME" [x]
  MkCon []  []    [2] -> prim "NULL" []
  MkCon [0] [x,y] []  -> prim "CONS" [x,y]

  c@(MkCon l xs _) -> prim name xs
   where
    name = shapeName (conShape c) <> "_CON_" <> (show . succ . length) l
