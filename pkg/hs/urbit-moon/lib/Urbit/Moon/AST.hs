{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Moon.AST
  ( AST(..)
  , Pat(..)
  , Con(..)
  , Bind(..)
  , astExample
  , astRune
  , astPrettyPrint
  ) where

import ClassyPrelude

import qualified Urbit.Runic as R

import Numeric.Natural (Natural)
import Prelude         ()
import Urbit.Moon.Prim (Op(..))
import Urbit.Moon.Lit  (Literal(..))
import Urbit.Runic     (Runic)



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
  | PAT Text AST [Pat]
  | OPR (Op AST)
  | LIT Literal
 deriving (Eq, Ord)


-- Instances -------------------------------------------------------------------

instance IsString AST where
    fromString = VAR . pack

instance Num AST where
    fromInteger = LIT . LitNat . fromIntegral
    (+)    = error "Fake Num instance for `Exp`"
    (-)    = error "Fake Num instance for `Exp`"
    (*)    = error "Fake Num instance for `Exp`"
    abs    = error "Fake Num instance for `Exp`"
    signum = error "Fake Num instance for `Exp`"
    negate = error "Fake Num instance for `Exp`"

{-
showLet :: [Bind] -> AST -> String
showLet binds x = "/=(" <> go binds <> show x <> ")"
 where
  go = \case
    []              -> ""
    Bind n t b : bs -> mconcat [unpack n, "/", show t, " ", show b, ", ", go bs]
-}

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

{-
showFunSeq :: ([(Text, AST)], AST) -> String
showFunSeq = \case
  ([], x) -> show x
  (bs, x) -> "{" <> intercalate " " (go bs x) <> "}"
 where
  go :: [(Text, AST)] -> AST -> [String]
  go []         x = [show x]
  go (("",t):bs) x = show t : go bs x
  go ((n,t):bs) x = (unpack n <> "/" <> show t) : go bs x
-}

appSeq :: AST -> AST -> [AST]
appSeq = \l r -> go [r] l
 where
  go acc (APP l r) = go (r:acc) l
  go acc node      = node : acc

instance Show Bind where
  show (Bind n t v) = "++  " <> unpack n <> "/" <> show t <> "  " <> show v

instance Show AST where
  show = unpack . R.runicShowWide


-- Helpers ---------------------------------------------------------------------

_boolTy :: AST
_boolTy = DAT [[],[]]

patRune :: Text -> AST -> [Pat] -> Runic
patRune v (DAT [[],[]]) [MkPat [] y, MkPat [] n] =
  R.RunC "?:" [R.Leaf v, astRune y, astRune n]
patRune v t pats =
  R.Jog1 "?-" (R.Bind v $ astRune t) (go <$> pats)
 where
  go :: Pat -> (Runic, Runic)
  go (MkPat vs body) = (R.IFix "[" "]" (R.Leaf <$> vs), astRune body)


-- Examples --------------------------------------------------------------------

tupCon :: [AST] -> [AST] -> AST
tupCon typs vals = CON (DAT [typs]) 0 vals

astExample :: AST
astExample = LET bindsEx $ tupCon [TYP 0, TYP 0] ["int", "nat"]

lam :: [Text] -> AST -> AST
lam [] x     = x
lam (v:vs) x = LAM v (lam vs x)

bindsEx :: [Bind]
bindsEx =
  [ Bind "type" t1     $ t0
  , Bind "sign" t0     $ INT
  , Bind "atom" t0     $ NAT
  , Bind "ideT" t0     $ taa
  , Bind "iden" "ideT" $ lam ["a", "x"] "x"
  , Bind "vect" tt     $ LAM "a" $ VEC "a"
  , Bind "list" tt     $ listTy
  , Bind "byte" t0     $ WOR 8
  , Bind "flag" t0     $ WOR 1
  , Bind "unit" t0     $ WOR 0
  , Bind "octs" t0     $ BUF 8
  , Bind "seqT" t0     $ tabb
  , Bind "seqV" "seqT" $ lam ["a", "b"] ("a" `SEQ` "b")
  , Bind "null" "list" $ LAM "a" $ CON ("list" `APP` "a") 0 []
  , Bind "conT" t0     $ consTy
  , Bind "cons" "conT" $ lam ["a", "x", "y"] $ CON ("list" `APP` "a") 1 ["x", "y"]
  , Bind "byt1" "byte" $ OPR $ WOR_NAT_INC 8 $ LIT $ LitWorNat 8 0
  , Bind "true" "flag" $ true
  , Bind "fals" "flag" $ false
  , Bind "emTy" t0     $ emptTy
  , Bind "empt" "emTy" $ lam ["a", "x"]
                       $ PAT "x" ("list" `APP` "a")
                       $ [MkPat [] "true", MkPat ["_", "_"] false]
  ]
 where
  true  = LIT $ LitWorNat 1 1
  false = LIT $ LitWorNat 1 0
  listTy = FUN "a" t0 $ DAT [[], ["a", "list" `APP` "a"]]
  consTy = FUN "a" t0
         $ FUN "" "a"
         $ FUN "" ("list" `APP` "a")
         $ "list" `APP` "a"
  emptTy = FUN "a" t0 $ FUN "" ("list" `APP` "a") "flag"
  t0 = TYP 0
  t1 = TYP 1
  tt = FUN "" t0 t0
  taa = FUN "a" t0 $ FUN "" "a" "a"
  tabb = FUN "a" t0 $ FUN "b" t0 $ FUN "" "a" $ FUN "" "b" "a"


-- Pretty Printing -------------------------------------------------------------

astPrettyPrint :: AST -> IO ()
astPrettyPrint = putStrLn . R.runicShow

instance R.ToRunic AST where
  toRunic = astRune

appRune :: [Runic] -> Runic
appRune xs = R.Mode wide tall
 where
  wide = R.IFix "(" ")" xs
  tall = case length xs of
           2 -> R.RunC "%-" xs
           _ -> R.RunN "%*" xs

conRune :: AST -> Nat -> [AST] -> Runic
conRune t n xs =
  case (n, t) of
    (0, DAT [_]) -> tupRune xs
    _            -> R.RunN "%%" (astRune t : R.Leaf (tshow n) : fmap astRune xs)

tupRune ∷ [AST] -> Runic
tupRune xs = R.IFix "[" "]" (astRune <$> xs)

bindRune :: Bind -> (Runic, Runic)
bindRune (Bind n t v) = (R.Bind n (astRune t), astRune v)

funSeqRune :: ([(Text, AST)], AST) -> Runic
funSeqRune (bs, x) = R.Mode wide tall
 where
  wide = R.IFix "{" "}" $ fmap binder bs <> [astRune x]
  tall = R.RunN "$-" $ fmap binder bs <> [astRune x]

binder :: (Text, AST) -> Runic
binder ("", v) = astRune v
binder (n,  v) = R.Bind n (astRune v)

lamRune :: [Text] -> AST -> Runic
lamRune vs x = R.Mode wid tal
 where
  wid = R.IFix "<" ">" (fmap R.Leaf vs <> [astRune x])
  tal = case vs of
          []  -> astRune x
          [v] -> R.RunC "|=" [R.Leaf v, astRune x]
          _   -> R.RunC "|=" [R.IFix "(" ")" (R.Leaf <$> vs), astRune x]

astRune ∷ AST → Runic
astRune = go
 where
  go = \case
    VAR t              -> R.Leaf t
    APP f x            -> appRune (go <$> appSeq f x)
    LAM v b            -> uncurry lamRune (lamSeq v b)
    LET bs x           -> case bs of
      []           -> astRune x
      [Bind n t v] -> R.RunC "=/" [R.Bind n (go t), go v, go x]
      _            -> R.Jog1 "|^" (go x) (bindRune <$> bs)
    SEQ x y   -> R.RunC "!!" [go x, go y]
    TYP n     -> R.Leaf (replicate (fromIntegral $ n+1) '*')
    FUN n t b -> funSeqRune (funSeq n t b)
    DAT [xs]  -> tupRune xs
    DAT cs    -> R.RunN "&&" (tupRune <$> cs)
    NAT       -> R.Leaf "@"
    INT       -> R.Leaf "!"
    VEC x     -> R.IFix "#[" "]" [go x] -- TODO Tall
    WOR 0     -> R.Leaf "~"
    WOR 1     -> R.Leaf "?"
    WOR x     -> R.Leaf ("@" <> tshow x)
    BUF n     -> R.Leaf ("#" <> tshow n)
    CON t n x -> conRune t n x
    PAT v t p -> patRune v t p
    OPR o     -> R.toRunic o
    LIT l     -> R.Leaf (tshow l)
