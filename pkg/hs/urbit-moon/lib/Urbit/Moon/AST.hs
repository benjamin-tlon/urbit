{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Moon.AST
  ( AST(..)
  , astExample
  , astRune
  , astPrettyPrint
  ) where

import ClassyPrelude

import qualified Urbit.Runic as R

import Control.Lens    (view, _1)
import Prelude         ()
import Urbit.Moon.Atom (Atom(..))
import Urbit.Moon.Prim (Op(..))
import Urbit.Runic     (Runic)


-- Types -----------------------------------------------------------------------

type Bind = (Text, [Text], AST)

data AST
  = VAR Text
  | APP AST AST
  | LAM Text AST
  | LET [(Text, AST)] AST
  | REC [(Text, [Text], AST)] AST
  | CON [Int] Int [AST]
  | PAT AST [([Text], AST)]
  | SEQ AST AST
  | LIT Atom
  | CAS AST [(Maybe Atom, AST)]
  | VEC [AST]
  | OPR (Op AST)
 deriving (Eq, Ord)


-- Instances -------------------------------------------------------------------

instance IsString AST where
    fromString = VAR . pack

instance Num AST where
    fromInteger = LIT . NAT . fromIntegral
    (+)    = error "Fake Num instance for `Exp`"
    (-)    = error "Fake Num instance for `Exp`"
    (*)    = error "Fake Num instance for `Exp`"
    abs    = error "Fake Num instance for `Exp`"
    signum = error "Fake Num instance for `Exp`"
    negate = error "Fake Num instance for `Exp`"

instance Show AST where
  show = unpack . R.runicShowWide


-- Helpers ---------------------------------------------------------------------

lamSeq :: Text -> AST -> ([Text], AST)
lamSeq = \v b -> go [v] b
 where
  go acc (LAM v b) = go (v:acc) b
  go acc node      = (reverse acc, node)

appSeq :: AST -> AST -> [AST]
appSeq = \l r -> go [r] l
 where
  go acc (APP l r) = go (r:acc) l
  go acc node      = node : acc


-- Examples --------------------------------------------------------------------

tup :: [AST] -> AST
tup xs = CON [length xs] 0 xs

astExample :: AST
astExample = modulE bindsEx

modulE :: [Bind] -> AST
modulE bs = REC bs $ tup $ fmap (VAR . view _1) bs

numsEx :: [Atom]
numsEx =
  [ NAT 99
  , INT 0
  , INT (-5)
  , WOR 0 0
  , WOR 1 1
  , WOR 8 255
  ]

bufsEx :: [Atom]
bufsEx =
  [ SYM "sym"
  , BUF 8 [3,4,5]
  ]

bindsEx :: [Bind]
bindsEx =
  [ (,,) "true"  []            (CON [0,0] 1 [])
  , (,,) "false" []            (CON [0,0] 0 [])
  , (,,) "vec0"  []            (VEC [])
  , (,,) "vec1"  ["x"]         (VEC ["x"])
  , (,,) "vec2"  ["x", "y"]    (VEC ["x", "y"])
  , (,,) "nums"  []            (VEC (LIT <$> numsEx))
  , (,,) "bufs"  []            (VEC (LIT <$> bufsEx))

  , (,,) "id"    ["x"]         "x"
  , (,,) "dup"   ["x"]         (CON [2] 0 ["x","x"])
  , (,,) "incw8" ["x"]         (OPR $ WOR_NAT_INC 8 "x")

  , (,,) "none"  []            (CON [0,1] 0 [])
  , (,,) "some"  ["x"]         (CON [0,1] 1 ["x"])
  , (,,) "opt"   ["n","s","x"] (PAT "x" [([], "n"), (["v"], "s" `APP` "v")])

  , (,,) "nil"   []            (CON [0,2] 0 [])
  , (,,) "cons"  ["x", "xs"]   (CON [0,2] 1 ["x", "xs"])
  , (,,) "null"  ["l"]         (PAT "l" [([], "true"), (["_","_"], "false")])
  , (,,) "seq"   ["x", "y"]    (SEQ "x" "y")
  ]


-- Pretty Printing -------------------------------------------------------------

astPrettyPrint :: AST -> IO ()
astPrettyPrint = putStrLn . R.runicShow

instance R.ToRunic AST where
  toRunic = astRune

paren :: [Text] -> Runic
paren = R.IFix "(" ")" . fmap R.Leaf

astRune ∷ AST → Runic
astRune = go
 where
  go = \case
    VAR t            -> R.Leaf t
    APP f x          -> appRune (go <$> appSeq f x)
    LAM v b          -> lam (lamSeq v b)
    LET []         x -> go x
    LET bs x         -> R.Cor1 "=/" (go x) (binV <$> bs)
    REC []         x -> go x
    REC bs         x -> R.Cor1 "|^" (go x) (bin <$> bs)
    CON [_] 0 xs     -> tupRune xs
    CON ns  i xs     -> R.Mode (R.IFix "%(" ")" par) (R.RunC "%%" par)
                         where
                          int = LIT . NAT . fromIntegral
                          par = fmap go [tup (int <$> ns), int i, tup xs]
    PAT x ps         -> R.Jog1 "?-" (go x) $ ps <&> \(v, b) ->
                          (R.IFix "[" "]" (R.Leaf <$> v), go b)
    OPR o            -> R.toRunic o
    LIT l            -> R.Leaf (tshow l)
    SEQ x y          -> R.IFix "!(" ")" [go x, go y]
    CAS x cs         -> R.Jog1 "?#" (go x) $ cs <&> \(v, b) ->
                          (fromMaybe "_" (R.toRunic <$> v), go b)
    VEC xs           -> R.Mode (R.IFix "#[" "]" (go <$> xs))
                               (R.RunN ":#" (go <$> xs))

  binV (n,v) = (R.Leaf n, go v)

  bin = \case
    (,,) n [] v -> (R.Leaf n, go v)
    (,,) n rs v -> (paren (n:rs), go v)

  lam (vs, x) = R.Mode wid tal
   where
    wid = R.IFix "<" ">" (fmap R.Leaf vs <> [astRune x])
    tal = case vs of
            []  -> astRune x
            [n] -> R.RunC "|=" [R.Leaf n, astRune x]
            _   -> R.RunC "|=" [paren vs, astRune x]

  appRune :: [Runic] -> Runic
  appRune xs = R.Mode wide tall
   where
    wide = R.IFix "(" ")" xs
    tall = case length xs of
             2 -> R.RunC "%-" xs
             3 -> R.RunC "%+" xs
             4 -> R.RunC "%^" xs
             _ -> R.RunN "%*" xs

  tupRune ∷ [AST] -> Runic
  tupRune xs = R.Mode (R.IFix "[" "]" (astRune <$> xs)) tal
   where
    tal = case xs of
      []        -> "[]"
      [_]       -> R.RunC ":." (astRune <$> xs)
      [_,_]     -> R.RunC ":-" (astRune <$> xs)
      [_,_,_]   -> R.RunC ":+" (astRune <$> xs)
      [_,_,_,_] -> R.RunC ":&" (astRune <$> xs)
      _         -> R.RunN ":*" (astRune <$> xs)
