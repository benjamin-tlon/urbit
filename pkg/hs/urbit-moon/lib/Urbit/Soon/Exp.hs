{-
  * TODO Decompilation:
  ** DONE Long-Step `simp`.
  ** DONE Really think through `simp`.
  ** TODO Quickcheck tests for jetFun <-> funJet

  * DONE Proof-Of-Concept Jet Optimizer
  ** DONE Change the `Jet` type to `(Nat, Nat, Skew, Exp)`
  ** DONE When "running" jets, run the `Exp` against a `Vector Skew`

  * TODO Syntax Trees
  ** DONE Design an AST type with support for bindings.
  ** DONE Convert AST -> Exp
  ** TODO Solve the problems of jets with the same name.
  ** TODO Consider using the same namespace for jets and variables.
  ** DONE Convert Exp -> AST

  * TODO Pretty Printer
  ** TODO Pretty Print ASTs
  ** TODO Convert Exp -> AST, Lifting nested functions.
  ** TODO Convert Exp -> AST, Lifting inlined jets.

  * TODO Front End
  ** TODO Parser for `Exp`.
  ** TODO Print values that reference jets as `|^` expressions.
  ** TODO Program runner.

  * Sample Programs
  ** Implement `fib`
  ** Implement `jam`
  ** Implement `odd` and `even` using mutual recursion.
-}

{-# OPTIONS_GHC -Wall #-} -- -Werror #-}

module Urbit.Soon.Exp
  ( Leaf(..)
  , Skew(..)
  , nf
  , whnf
  , Exp(..)
  , compile
  , funJet
  , jetFun
  , jetSkew
  , tuple
  , brak
  , saturated
  , subst

  , Prog(..)
  , Bind(..)
  , AST(..)
  , prettyPrintAST
  , progExp
  , expProg

  , oleg

  , pattern S
  , pattern K
  , pattern E
  , pattern W
  , pattern B
  , pattern Q
  , pattern U
  , pattern I
  , pattern C
  ) where

import ClassyPrelude hiding (bracket)
import Numeric.Natural
import Control.Arrow

import Data.List   (iterate, elemIndex, (!!))
import Data.Vector ((!))
import Urbit.Atom  (atomUtf8, utf8Atom)

type Nat = Natural


--------------------------------------------------------------------------------

pattern S, K, E, W, B, Q, U, I, C :: (Eq n, Num n) => n
pattern S = 0 -- distribute
pattern K = 1 -- const
pattern E = 2 -- ehhance
pattern W = 3 -- which
pattern B = 4 -- compose
pattern Q = 5 -- seq
pattern U = 6 -- succ
pattern I = 7 -- id
pattern C = 8 -- flip


-- SKEW Trees ------------------------------------------------------------------

data Jet = Jet Nat Nat Skew Exp
 deriving (Generic, NFData)

instance Eq Jet where
  Jet n1 t1 b1 _ == Jet n2 t2 b2 _ = (n1,t1,b1) == (n2,t2,b2)

instance Ord Jet where
  compare (Jet n1 t1 b1 _) (Jet n2 t2 b2 _) = compare (n1,t1,b1) (n2,t2,b2)

instance Show Jet where
  show (Jet n t b _) =
    "{" <> intercalate " " [show n, show (T t), show b] <> "}"

data Leaf
  = T Nat --  Atom
  | J Jet --  Loaded Function
 deriving (Eq, Ord, Generic, NFData)

data Skew = N Leaf [Skew]
 deriving (Eq, Ord, Generic, NFData)

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
  show (J j) = show j
  show (T n) = case atomUtf8 n of
                 Right t | isSym (unpack t) -> "%" <> unpack t
                 _                          -> show n

instance Show Skew where
  show (N n []) = show n
  show (N n xs) = "(" <> intercalate " " (show n:fmap show (reverse xs)) <> ")"


-- Super Combinators -----------------------------------------------------------

type Fun = (Nat, Nat, Exp)

data Exp
  = V Nat          --  Variable Reference
  | F Nat Nat Exp  --  Super-Combinator Reference
  | A Exp Exp      --  Function Application
  | L Skew         --  Skew literal
 deriving (Eq, Ord, Generic, NFData)

instance Show Exp where
  show (V n)     = "$" <> show n
  show (F n t b) = "{{" <> intercalate " " [show n,show (T t),show b] <> "}}"
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

brak :: Nat -> Exp -> Skew
brak args = \x -> cvt (go args x)
 where
  cvt (V _)     = error "variable reference left uncompiled"
  cvt (F n t b) = NT E % NT n % NT t % brak n b
  cvt (L l)     = l
  cvt (A x y)   = cvt x % cvt y

  go 0 x = x
  go n x = ab (args-n) $ go (n-1) x

  ab _ (L l)          = K `A` L l
  ab _ (F n t b)      = K `A` F n t b
  ab i (V v) | v==i   = S `A` K `A` K
  ab _ (V v)          = K `A` (V v)
  ab i b | noref i b  = K `A` b
  ab i (A x y)        = S `A` ab i x `A` ab i y

  noref i (V n)   = n /= i
  noref i (A x y) = noref i x && noref i y
  noref _ _       = True

compile :: Exp -> Skew
compile = brak 0

funJet :: Fun -> Jet
funJet (n,t,b) = Jet n t skew expr
 where
  expr = decompile n skew
  skew = nf (brak n b)

jetSkew :: Jet -> Skew
jetSkew (Jet n t b _) = E % NT n % NT t % b

funSkew :: Fun -> Skew
funSkew = jetSkew . funJet


-- SKEW Evaluation -------------------------------------------------------------

arity :: Leaf -> Int
arity (J (Jet n _ _ _)) = fromIntegral n
arity (T S)             = 3
arity (T K)             = 2
arity (T E)             = 3
arity (T W)             = 4
arity (T B)             = 3
arity (T Q)             = 2
arity (T U)             = 1
arity (T I)             = 1
arity (T C)             = 3
arity (T _)             = 1

nf :: Skew -> Skew
nf (whnf -> N h xs) = N h (nf <$> xs)

whnf :: Skew -> Skew
whnf = go
 where
  go = \case
    N (J (Jet n _ _ x)) xs | length xs == fromIntegral n ->
      go $ subst (fromList $ reverse xs) x

    N (T S) [z,y,x] -> go (x % z % (y % z))
    N (T K) [_,x]   -> go x

    N (T E) [b,t,n] -> case (whnf n, whnf t) of
      ( NT 0,  NT _  ) -> nf b
      ( NT nv, NT tv ) -> N (J (Jet nv tv bv (decompile nv bv))) []
                           where bv = nf b
      ( _,     NT _  ) -> error "arity not atom"
      ( _,     _     ) -> error "tag not atom"

    N (T W) [x,z,a,c] -> case whnf x of
      N (T 0) []             -> go z
      N (T n) []             -> go (a % NT(n-1))
      N (J (Jet n t b _)) [] -> go (c % (NT E % NT n % NT t) % b)
      N n (v:vs)             -> go (c % (N n vs) % v)

    N (T B) [c,b,a] -> go (a % (b % c))

    N (T Q) [y,x]   -> go x `seq` go y
    N (T Q) [y,x]   -> go x `seq` go y

    N (T U) [x] -> case (whnf x) of
      NT n -> NT (n+1)
      _    -> error "inc not atom"

    N (T I) [a]     -> go a
    N (T C) [c,b,a] -> go (a % c % b)

    N x []                       -> N x []
    N x xs | length xs < arity x -> N x xs
    N (T n) _ | n>8              -> error ("bad opcode: " <> show n)
    N n (x:xs)                   -> go (go (N n xs) % x)


-- Decompilation ---------------------------------------------------------------

decompile :: Nat -> Skew -> Exp
decompile args = wind . simp . unwind . go args . L
 where
  go :: Nat -> Exp -> Exp
  go 0 x = x
  go n x = go (n-1) (x `A` V (args - n))

jetFun :: Jet -> Fun
jetFun (Jet arg tag _ xpr) = (arg, tag, xpr)

simp :: Exp -> Exp
simp = simpWhnf >>> \case
  A x y  -> A (simp x) (simp y)
  done   -> done

simpWhnf :: Exp -> Exp
simpWhnf = go
 where
  go expr = case expr of
    L _                      -> expr
    V _                      -> expr
    F _ _ _                  -> expr
    A (L _) _                -> expr
    A (V _) _                -> expr
    A (F _ _ _) _            -> expr
    L 0 `A` x `A` y `A` z    -> go (x `A` z `A` (y `A` z))
    L 1 `A` x `A` _          -> go x
    _ | not (saturated expr) -> expr
    x `A` y `A` z            -> go (go (x `A` y) `A` z)

saturated :: Exp -> Bool
saturated = sat 0
 where
  sat :: Int -> Exp -> Bool
  sat have = \case
    L (N t@(T S) xs) -> have >= need where need = arity t - length xs
    L (N t@(T K) xs) -> have >= need where need = arity t - length xs
    L _              -> False
    V _              -> False
    A x _            -> sat (have+1) x
    F _ _ _          -> False -- Don't simplify jets.

unwind :: Exp -> Exp
unwind (V n)         = V n
unwind (F n t b)     = F n t b
unwind (A x y)       = A (unwind x) (unwind y)
unwind (L (N x xs))  = foldl' A (L (N x [])) (unwind . L <$> reverse xs)

wound :: Exp -> Bool
wound (V _)           = True
wound (F _ _ _)       = True
wound (L _)           = True
wound (A (L _) (L _)) = False
wound (A x y)         = wound x && wound y

wind :: Exp -> Exp
wind (A (L x) (L y)) = L (x % y)
wind x | wound x     = x
wind (A x y)         = wind (A (wind x) (wind y))
wind x               = x


-- Jet Evaluation --------------------------------------------------------------

subst :: Vector Skew -> Exp -> Skew
subst args expr = go expr
 where
  go :: Exp -> Skew
  go = \case
    V v           -> args ! fromIntegral v
    L x           -> x
    F n t b       -> funSkew (n,t,b) -- Shouldn't happen.
    A (L x) (L y) -> x % y
    A x y         -> go x % go y


-- Parsing and Printing --------------------------------------------------------

data AST
  = JET Text -- Reference to jet by position.
  | VAR Text -- Argument reference.
  | APP AST AST
  | NAT Nat
 deriving (Eq, Ord, Show, Generic, NFData)

data Bind = Bind Text [Text] AST
  deriving (Show)

data Prog = Prog [Bind] AST
  deriving (Show)

textNat :: Text -> Nat
textNat = utf8Atom

_textSkew :: Text -> Skew
_textSkew n = N (T $ textNat n) []

natText :: Nat -> Either Text Text
natText = either (Left . tshow) Right . atomUtf8

bindName :: Bind -> Text
bindName (Bind n _ _) = n

progExp :: Prog -> Maybe Exp
progExp (Prog binds expr) = go [] expr
 where
  go :: [Text] -> AST -> Maybe Exp
  go args = \case
    APP x y -> A <$> go args x <*> go args y
    VAR v   -> V . fromIntegral <$> elemIndex v args
    NAT n   -> Just $ L $ NT n
    JET j   -> do Bind t xs b <- find ((==j) . bindName) binds
                  bexp <- go xs b
                  let n = fromIntegral $ length xs
                  guard (n /= 0)
                  pure $ F n (textNat t) bexp

-- TODO: infinite list
allVars :: [Text]
allVars = singleton <$> ['a'..'z']

expProg :: Exp -> IO Prog
expProg expr = do
  jets <- newIORef (mempty :: Map Text Bind)

  let reg :: Register
      reg arg nam bod = do
        tbl <- readIORef jets

        case lookup nam tbl of
               Nothing -> pure ()
               Just _  -> error ("name conflict, can't print: %" <> unpack nam)

        bodAst <- expAST reg bod

        let nams = (allVars !!) . fromIntegral <$> [0 .. arg-1]
        let bind = Bind nam nams bodAst

        writeIORef jets (insertMap nam bind tbl)

  body <- expAST reg expr
  Prog <$> (toList <$> readIORef jets) <*> pure body

type Register = Nat -> Text -> Exp -> IO ()

{-
  * Let's not do dedulication here
  ** Just wait until it's done in the runtime system.
  ** Instead, insert into the map by name.
  ** Crash if duplicate names.
  ** Return JET node from name.
-}
funAST :: Register -> Fun -> IO AST
funAST reg (arg, tag, bod) = do
  nm <- either badName pure (natText tag)
  reg arg nm bod
  pure (JET nm)
 where
  badName :: a -> IO Text
  badName _ = error ("Can't print jet with name: " <> show tag)

expAST :: Register -> Exp -> IO AST
expAST reg = \case
  V n     -> pure $ VAR (allVars !! fromIntegral n)
  L n     -> skewAST reg n
  A x y   -> APP <$> expAST reg x <*> expAST reg y
  F n t b -> funAST reg (n, t, b)

skewAST :: Register -> Skew -> IO AST
skewAST reg (N l xs) = do
  a  <- leafAST reg l
  as <- traverse (skewAST reg) (reverse xs)
  pure (foldl' APP a as)

leafAST :: Register -> Leaf -> IO AST
leafAST _   (T t)             = pure (NAT t)
leafAST reg (J (Jet n t _ x)) = funAST reg (n,t,x)

prettyPrintAST :: AST -> IO ()
prettyPrintAST = error "TODO"

-- Testing ---------------------------------------------------------------------

tuple :: Nat -> Fun
tuple 0 = (1, textNat "tup0", V 0)
tuple n = (n+1, textNat ("tup" <> tshow n), foldl' A f xs)
 where
  f  = V n
  xs = V <$> [0..n-1]


-- Oleg ------------------------------------------------------------------------

data Deb = Zero | Succ Deb | Lam Deb | App Deb Deb | Dit Skew deriving Show

oleg :: Nat -> Exp -> Skew
oleg args body = debSkew $ wrap args $ go body
 where
  wrap :: Nat -> Deb -> Deb
  wrap n x = iterate Lam x !! fromIntegral n

  go :: Exp -> Deb
  go = \case
    V n     -> iterate Succ Zero !! fromIntegral n
    F x y z -> Dit (funSkew (x,y,z))
    A x y   -> App (go x) (go y)
    L l     -> Dit l

debSkew :: Deb -> Skew
debSkew = snd . go
 where
  go = \case
    Dit x                         -> (0,       x)
    Zero                          -> (1,       I)
    Succ d    | x@(n, _) <- go d  -> (n + 1,   f (0, 1) x)
    App d1 d2 | x@(a, _) <- go d1
              , y@(b, _) <- go d2 -> (max a b, f x y)
    Lam d | (n, e) <- go d -> case n of
                                0 -> (0,       1 % e)
                                _ -> (n - 1,   e)

  f (a, x) (b, y) = case (a, b) of
    (0, 0)             ->        x % y
    (0, n)             -> bn n % x % y
    (n, 0)             -> cn n % x % y
    (n, m) | n == m    -> sn n % x % y
           | n < m     ->               bn (m - n) % (sn n % x) % y
           | otherwise -> cn (n - m) % (bn (n - m) %  sn m % x) % y

  bn i = iterate ((B%B)%) B !! (i-1)
  cn i = iterate ((B%(B%C)%B)%) C !! (i-1)
  sn i = iterate ((B%(B%S)%B)%) S !! (i-1)
