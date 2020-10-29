{-
  * Cleanup
  ** DONE Remove `F` constructor.
  ** DONE Refactor Skew -> AST conversion
  ** DONE Deduplicate jets.
  ** DONE Correctly handle generated variables names conflicting with jet names.

  * DONE Decompilation:
  ** DONE Long-Step `simpNf`.
  ** DONE Really think through `simpNf`.

  * DONE Proof-Of-Concept Jet Optimizer
  ** DONE Change the `Jet` type to `(Nat, Nat, Skew, Exp)`
  ** DONE When "running" jets, run the `Exp` against a `Vector Skew`

  * Syntax Trees
  ** DONE Design an AST type with support for bindings.
  ** DONE Convert AST -> Exp
  ** DONE Solve the problems of jets with the same name.
  ** DONE Consider using the same namespace for jets and variables.
  ** DONE Convert Exp -> AST

  * DONE Pretty Printer
  ** DONE Pretty Print ASTs
  ** DONE Convert Exp -> AST, lifting jets.

  * Front End
  ** TODO Parser for `AST`.
  ** TODO Evaluate Text
  ** TODO REPL
  ** TODO Interactive programs: `data Main :: Main (Nat -> (Nat, Main))`

  * Sample Programs
  ** TODO Implement `fib`
  ** TODO Implement `odd` and `even` using mutual recursion.
  ** TODO Implement `jam`
  ** TODO Implement `soon`?
-}

{-# OPTIONS_GHC -Wall #-}

module Urbit.Soon.Exp
  ( Leaf(..)
  , Skew(..)
  , nf
  , whnf
  , Exp(..)
  , Jet(..)
  , getJet
  , putJet
  , Fun
  , compile
  , funJet
  , jetFun
  , jetSkew

  , Reg(..)
  , registrations

  , tuple, tagged

  , oleg

  , pattern NT
  , pattern S
  , pattern K
  , pattern E
  , pattern W
  , pattern B
  , pattern Q
  , pattern U
  , pattern D
  , pattern I
  , pattern C

  , expDeb
  , funSkew
  , funDeb
  ) where

import ClassyPrelude        hiding (bracket, many, some, try)
import Control.Arrow
import Numeric.Natural

import Data.List        (iterate, (!!))
import Data.Vector      ((!))
import System.IO.Unsafe (unsafePerformIO)
import Urbit.Atom       (atomUtf8, utf8Atom)

type Nat = Natural


--------------------------------------------------------------------------------

pattern S, K, E, W, B, Q, U, I, C, D :: (Eq n, Num n) => n
pattern S = 0 -- distribute
pattern K = 1 -- const
pattern I = 2 -- id
pattern B = 3 -- compose
pattern C = 4 -- flip
pattern Q = 5 -- seq
pattern E = 6 -- ehhance
pattern W = 7 -- which
pattern U = 8 -- succ
pattern D = 9 -- succ


-- SKEW Trees ------------------------------------------------------------------

data Jet = Jet Nat Nat Skew Exp
 deriving (Generic, NFData, Hashable)

instance Eq Jet where
  Jet n1 t1 b1 _ == Jet n2 t2 b2 _ = (n1,t1,b1) == (n2,t2,b2)

instance Ord Jet where
  compare (Jet n1 t1 b1 _) (Jet n2 t2 b2 _) = compare (n1,t1,b1) (n2,t2,b2)

instance Show Jet where
  show (Jet n t b _) =
    "{" <> intercalate " " [show n, show (T t), show b] <> "}"

data Leaf
  = T Nat --  Atom
  | J Int --  Loaded Function
 deriving (Eq, Ord, Generic, NFData, Hashable)

data Skew = N Leaf [Skew]
 deriving (Eq, Ord, Generic, NFData, Hashable)

pattern NT :: Nat -> Skew
pattern NT n = N (T n) []

-- Function Application
infixl 5 %;
(%) :: Skew -> Skew -> Skew
(%) (N n xs) x = N n (x:xs)

instance Num Skew where
  fromInteger x = N (T (fromIntegral x)) []
  (+)    = error "hack"
  (-)    = error "hack"
  (*)    = error "hack"
  abs    = error "hack"
  signum = error "hack"
  negate = error "hack"

isSym :: String -> Bool
isSym []  = False
isSym [_] = False
isSym (x:xs) = elem x ['a'..'z'] && all (`elem` symChr) xs
 where
  symChr = ("-" <> ['0'..'9'] <> ['a'..'z'])

instance Show Leaf where
  show (J (getJet -> Jet n t _ x)) =
    "{" <> intercalate " " [show n,show (T t),show x] <> "}"
  show (T n) =
    case atomUtf8 n of
     Right t | isSym (unpack t) -> "%" <> unpack t
     _                          -> show n

instance Show Skew where
  show (N n []) = show n
  show (N n xs) = "(" <> intercalate " " (show n:fmap show (reverse xs)) <> ")"


-- Super Combinators -----------------------------------------------------------

type Fun = (Nat, Nat, Exp)

data Exp
  = V Nat          --  Variable Reference
  | A Exp Exp      --  Function Application
  | L Skew         --  Skew literal
 deriving (Eq, Ord, Generic, NFData, Hashable)

instance Show Exp where
  show (V n)     = "$" <> show n
  show (L l)     = ":" <> show l
  show (A x y)   = "(" <> intercalate " " (appS x [y]) <> ")"
   where
    appS :: Exp -> [Exp] -> [String]
    appS (A p q) rs = appS p (q:rs)
    appS n       rs = fmap show (n:rs)

instance Num Exp where
  fromInteger = L . fromInteger
  (+)    = error "hack"
  (-)    = error "hack"
  (*)    = error "hack"
  abs    = error "hack"
  signum = error "hack"
  negate = error "hack"


-- Compile Super-Combinator To SKEW --------------------------------------------

compile :: Exp -> Skew
compile = oleg 0

funJet :: Fun -> Jet
funJet (n,t,b) = Jet n t skew expr
 where
  expr = decompile n skew
  skew = nf (oleg n b)

jetSkew :: Jet -> Skew
jetSkew (Jet n t b _) = E % NT n % NT t % b

funSkew :: Fun -> Skew
funSkew = jetSkew . funJet


-- SKEW Evaluation -------------------------------------------------------------

data Reg = MkReg
  { regNex :: !Int
  , regTbl :: !(IntMap Jet)
  , regMap :: !(HashMap Jet Int)
  }

registrations :: MVar Reg
registrations = unsafePerformIO $ newMVar $ MkReg 0 mempty mempty

getJet :: Int -> Jet
getJet i = fromMaybe (error "invalid jet number")
         $ lookup i
         $ regTbl
         $ unsafePerformIO
         $ readMVar registrations

putJet :: Jet -> Int
putJet j = unsafePerformIO $ do
  evaluate (force j)
  reg@(MkReg nex tbl lbt) <- takeMVar registrations
  case lookup j lbt of
    Just i -> do
      putMVar registrations reg
      pure i
    Nothing -> do
      putMVar registrations
        $ MkReg (nex+1) (insertMap nex j tbl) (insertMap j nex lbt)
      pure nex

arity :: Leaf -> Int
arity (J (getJet -> Jet n _ _ _)) = fromIntegral n
arity (T t) = case t of
  S -> 3
  K -> 2
  I -> 1
  B -> 3
  C -> 3
  Q -> 2
  E -> 3
  W -> 3
  U -> 1
  D -> 3
  _ -> 1

nf :: Skew -> Skew
nf (whnf -> N h xs) = N h (nf <$> xs)

whnf :: Skew -> Skew
whnf = go
 where
  go = \case
    N (J (getJet -> Jet n t _ x)) xs | length xs == fromIntegral n ->
      go $ subst t (fromList $ reverse xs) x

    N (T S) [z,y,x] -> go (x % z % (y % z))
    N (T K) [_,x]   -> go x
    N (T I) [a]     -> go a
    N (T B) [c,b,a] -> go (a % (b % c))
    N (T C) [c,b,a] -> go (a % c % b)
    N (T Q) [y,x]   -> go x `seq` go y

    N (T E) [b,t,n] -> case (whnf n, whnf t) of
      ( NT 0,  NT _  ) -> error "Jet with arity 0"
      ( NT nv, NT tv ) -> N (J (putJet (Jet nv tv bv (decompile nv bv)))) []
                           where bv = nf b
      ( _,     NT _  ) -> error "arity not atom"
      ( _,     _     ) -> error "tag not atom"

    N (T W) [x,a,c] -> case whnf x of
      N n (v:vs)                       -> go (c % (N n vs) % v)
      N (J (getJet -> Jet n t b _)) [] -> go (c % (NT E % NT n % NT t) % b)
      N (T n) []                       -> go (a % NT n)


    N (T U) [x] -> case (whnf x) of
      NT n -> NT (n+1)
      _    -> error "oper 8: argument not atom"

    N (T D) [x,z,f] -> case (whnf x) of
      NT i -> go (iterate (f%) z !! fromIntegral i)
      _    -> error "oper 9: third argument not atom"

    N x []                       -> N x []
    N x xs | length xs < arity x -> N x xs
    N (T n) _ | n>9              -> error ("bad opcode: " <> show n)
    N n (x:xs)                   -> go (go (N n xs) % x)

subst :: Nat -> Vector Skew -> Exp -> Skew
subst _tag args expr = go expr
 where
  go :: Exp -> Skew
  go = \case
    V v           -> args ! fromIntegral v
    L x           -> x
    A (L x) (L y) -> x % y
    A x y         -> go x % go y


-- Compilation (Super-Combinators to Skew) -------------------------------------

data Deb = Zero | Succ Deb | Lam Deb | App Deb Deb | Dit Skew
 deriving Show

funDeb :: Fun -> Deb
funDeb (n,_,b) = expDeb n b

oleg :: Nat -> Exp -> Skew
oleg args body = debSkew (expDeb args body)

expDeb :: Nat -> Exp -> Deb
expDeb = top
 where
  top 0 body           = go 0 body
  top args (A h (V v)) | v == args-1 && not (refs (args-1) h)
                       = expDeb (args-1) h
  top args body        = iterate Lam (go args body) !! fromIntegral args

  go :: Nat -> Exp -> Deb
  go args = \case
    V n     -> iterate Succ Zero !! fromIntegral (args - (n+1))
    A x y   -> App (go args x) (go args y)
    L l     -> Dit l

refs :: Nat -> Exp -> Bool
refs i = \case
  V v     -> v == i
  A x y   -> refs i x || refs i y
  _       -> False

debSkew :: Deb -> Skew
debSkew = snd . go
 where
  go = \case
    Dit x                        -> (0,   x)
    Zero                         -> (1,   I)
    Succ d    | x@(n,_) <- go d  -> (n+1, f (0,K) x)
    App d1 d2 | x@(a,_) <- go d1
              , y@(b,_) <- go d2 -> (max a b, f x y)
    Lam d     | (n,e)   <- go d  -> case n of 0 -> (0,   K%e)
                                              _ -> (n-1, e)

  f (a,x) (b,y) = case (a,b) of
    (0,0)             ->        x % y
    (0,n)             -> bn n % x % y
    (n,0)             -> cn n % x % y
    (n,m) | n == m    -> sn n % x % y
          | n < m     ->             bn (m-n) % (sn n % x) % y
          | otherwise -> cn (n-m) % (bn (n-m) %  sn m % x) % y

  bn i = iterate ((B%B)%) B !! (i-1)
  cn i = iterate ((B%(B%C)%B)%) C !! (i-1)
  sn i = iterate ((B%(B%S)%B)%) S !! (i-1)


-- Decompilation (Skew Jets to Super-Combinators) ------------------------------

decompile :: Nat -> Skew -> Exp
decompile args = wind . simpNf . unwind . go args . L
 where
  go :: Nat -> Exp -> Exp
  go 0 x = x
  go n x = go (n-1) (x `A` V (args - n))

unwind :: Exp -> Exp
unwind (V n)         = V n
unwind (A x y)       = A (unwind x) (unwind y)
unwind (L (N x xs))  = foldl' A (L (N x [])) (unwind . L <$> reverse xs)

wind :: Exp -> Exp
wind (A (L x) (L y)) = L (x % y)
wind x | wound x     = x
wind (A x y)         = wind (A (wind x) (wind y))
wind x               = x

wound :: Exp -> Bool
wound (V _)           = True
wound (L _)           = True
wound (A (L _) (L _)) = False
wound (A x y)         = wound x && wound y

simpNf :: Exp -> Exp
simpNf = simpWhnf >>> \case
  A x y  -> A (simpNf x) (simpNf y)
  done   -> done

simpWhnf :: Exp -> Exp
simpWhnf = go
 where
  go expr = case expr of
    L S `A` a `A` b `A` c    -> go (a `A` c `A` (b `A` c))
    L K `A` a `A` _          -> go a
    L I `A` a                -> go a
    L C `A` a `A` b `A` c    -> go (a `A` c `A` b)
    L B `A` a `A` b `A` c    -> go (a `A` (b `A` c))
    L _ `A` _                -> expr
    V _ `A` _                -> expr
    L _                      -> expr
    V _                      -> expr
    _ | not (saturated expr) -> expr
    a `A` b `A` c            -> go (go (a `A` b) `A` c)

saturated :: Exp -> Bool
saturated = sat 0
 where
  sat :: Int -> Exp -> Bool
  sat have = \case
    L (N t@(T S) xs) -> have >= need where need = arity t - length xs
    L (N t@(T K) xs) -> have >= need where need = arity t - length xs
    L (N t@(T I) xs) -> have >= need where need = arity t - length xs
    L (N t@(T C) xs) -> have >= need where need = arity t - length xs
    L (N t@(T B) xs) -> have >= need where need = arity t - length xs
    L _              -> False
    V _              -> False
    A x _            -> sat (have+1) x

jetFun :: Jet -> Fun
jetFun (Jet arg tag _ xpr) = (arg, tag, xpr)


-- Testing ---------------------------------------------------------------------

tuple :: Nat -> Fun
tuple 0 = (1, utf8Atom "tup0", V 0)
tuple n = (n+1, utf8Atom ("tup" <> tshow n), foldl' A f xs)
 where
  f  = V n
  xs = V <$> [0..n-1]

tagged :: Nat -> Nat -> Fun
tagged 0 _        = error "no constructors for void type"
tagged n m | m>=n = error "constructor out of range."
tagged n m        = (n+1, utf8Atom nm, A f x)
 where
  nm = "tag" <> tshow m <> "-" <> tshow n
  f = V (1+m)
  x = V 0
