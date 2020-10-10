{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Moon.AST
  ( AST(..)
  , Pat(..)
  , Con(..)
  , Bind(..)
  , astExample
  ) where

import Prelude ()
import ClassyPrelude
import GHC.Natural
import Urbit.Moon.Prim (Op(..))


-- Types -----------------------------------------------------------------------

type Nat = Natural

data Bind = Bind Text AST AST
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
  | TYP Nat
  | FUN Text AST AST
  | DAT [[AST]]
  | NAT
  | INT
  | VEC AST
  | WOR Nat
  | BUF Nat
  | CON AST Nat [AST]
  | PAT AST AST [Pat]
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
    []              -> ""
    Bind n t b : bs -> mconcat [unpack n, "/", show t, " ", show b, ", ", go bs]

lamSeq :: Text -> AST -> ([Text], AST)
lamSeq = \v1 -> go [v1]
 where
  go acc (LAM v b) = go (v:acc) b
  go acc node      = (reverse acc, node)

funSeq :: Text -> AST -> AST -> ([(Text, AST)], AST)
funSeq = \n t b -> go [(n,t)] b
 where
  go acc (FUN n t b) = go ((n,t):acc) b
  go acc node        = (reverse acc, node)

showFunSeq :: ([(Text, AST)], AST) -> String
showFunSeq = \case
  ([], x) -> show x
  (bs, x) -> "{" <> intercalate " " (go bs x) <> "}"
 where
  go :: [(Text, AST)] -> AST -> [String]
  go []         x = [show x]
  go (("",t):bs) x = show t : go bs x
  go ((n,t):bs) x = (unpack n <> "/" <> show t) : go bs x

appSeq :: AST -> AST -> [AST]
appSeq = \l r -> go [r] l
 where
  go acc (APP l r) = go (r:acc) l
  go acc node      = node : acc

instance Show Bind where
  show (Bind n t v) = "++  " <> unpack n <> "/" <> show t <> "  " <> show v

instance Show AST where
  show = \case
    VAR t     -> unpack t
    APP f x   -> paren (appSeq f x)
    LAM v b   -> let (vs, bod) = lamSeq v b
                 in "<" <> intercalate " " (fmap unpack vs <> [show bod]) <> ">"
    LET bs x  -> showLet bs x
    SEQ x y   -> mconcat ["!!(", show x, " ", show y, ")"]
    TYP n     -> replicate (fromIntegral $ n+1) '*'
    FUN n t b -> showFunSeq (funSeq n t b)
    DAT cs    -> "&&" <> parenStr (brak <$> cs)
    NAT       -> "@"
    INT       -> "!"
    VEC x     -> "#" <> brak [x]
    WOR 0     -> "~"
    WOR 1     -> "?"
    WOR x     -> "@" <> show x
    BUF n     -> "#" <> show n
    CON t n x -> showCon t n x
    PAT x y p -> showPat x y p
    OPR oper  -> show oper


-- Helpers ---------------------------------------------------------------------

showPat :: AST -> AST -> [Pat] -> String
showPat _ x [MkPat [] y, MkPat [] n] =
  "?:(" <> show x <> " " <> show y <> " " <> show n <> ")"
showPat t x pats =
  "?-(" <> show t <> " " <> show x <> go pats <> ")"
 where
  go []                   = ""
  go (MkPat vs body:more) = mconcat [ ", ", vars vs, " ", show body, go more ]

parenStr :: [String] -> String
parenStr xs = "(" <> intercalate " " xs <> ")"

paren :: Show a => [a] -> String
paren xs = parenStr (show <$> xs)

brak :: [AST] -> String
brak xs = "[" <> intercalate " " (show <$> xs) <> "]"

vars :: [Text] -> String
vars xs = "[" <> intercalate " " (unpack <$> xs) <> "]"


-- Generalized ADT Constructors ------------------------------------------------

showCon :: AST -> Nat -> [AST] -> String
showCon t n xs = "%%" <> parenStr (show t : show n : fmap show xs)


-- Examples --------------------------------------------------------------------

tupCon :: [AST] -> [AST] -> AST
tupCon typs vals = CON (DAT [typs]) 0 vals

astExample :: AST
astExample = LET bindsEx $ tupCon [TYP 0, TYP 0] ["int", "nat"]

bindsEx :: [Bind]
bindsEx =
  [ Bind "type" t1     $ t0
  , Bind "sign" t0     $ INT
  , Bind "atom" t0     $ NAT
  , Bind "iden" taa    $ LAM "x" "x"
  , Bind "vect" tt     $ LAM "a" $ VEC "a"
  , Bind "list" tt     $ listTy
  , Bind "byte" t0     $ WOR 8
  , Bind "flag" t0     $ WOR 1
  , Bind "unit" t0     $ WOR 0
  , Bind "octs" t0     $ BUF 8
  , Bind "sequ" tabb   $ LAM "a" $ LAM "b" $ "a" `SEQ` "b"
  , Bind "null" listTy $ CON listTy 0 []
  , Bind "cons" consTy $ LAM "a" $ LAM "x" $ LAM "y" $ CON listTy 1 ["x", "y"]
  , Bind "byt1" "byte" $ OPR $ WOR_NAT_INC 8 $ OPR $ WOR_NAT_LIT 8 0
  , Bind "true" "flag" $ true
  , Bind "fals" "flag" $ false
  , Bind "empt" emptTy $ LAM "a"
                       $ LAM "x"
                       $ PAT ("list" `APP` "a") "x"
                       $ [MkPat [] "true", MkPat ["_", "_"] false]
  ]
 where
  true  = OPR $ WOR_NAT_LIT 1 1
  false = OPR $ WOR_NAT_LIT 1 0
  listTy = LAM "a" $ DAT [[], ["a", "list" `APP` "a"]]
  consTy = FUN "a" t0
         $ FUN "x" "a"
         $ FUN "" ("list" `APP` "a")
         $ "list" `APP` "a"
  emptTy = LAM "a" $ FUN "" ("list" `APP` "a") "flag"
  t0 = TYP 0
  t1 = TYP 1
  tt = FUN "" t0 t0
  taa = LAM "a" (FUN "" "a" "a")
  tabb = LAM "a" $ LAM "b" $ FUN "" "a" $ FUN "" "b" "a"
