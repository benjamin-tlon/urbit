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
  , compile
  , funJet
  , jetFun
  , jetSkew

  , Reg(..)
  , registrations

  , tuple, tagged

  , Prog(..)
  , Bind(..)
  , AST(..)
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
  , pattern D
  , pattern I
  , pattern C

  , expDeb
  , funSkew
  , funDeb
  , progPretty

  , prog
  , repl
  , execSoon

  , ace, pal, par, comment, bulkSpace, gap, whitespace, soonFile, parseAST
  , parseSoonTest
  ) where

import ClassyPrelude        hiding (bracket, many, some, try)
import Control.Arrow
import Numeric.Natural
import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Monad.State.Lazy (StateT, evalStateT, get, put)
import Data.Function            ((&))
import Data.List                (elemIndex, iterate, (!!))
import Data.Vector              ((!))
import Data.Void                (Void)
import System.IO.Unsafe         (unsafePerformIO)
import Urbit.Atom               (atomUtf8, utf8Atom)

import qualified Prelude
import qualified Urbit.Runic as R

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

indent :: String -> String
indent = unlines . fmap ("    " <>) . lines

firstLine :: String -> String
firstLine inpu = case lines inpu of
  []     -> ""
  (x:xs) -> x

subst :: Nat -> Vector Skew -> Exp -> Skew
subst tag args expr = go expr
 where
  -- dbug = show (T tag) <> "\n" <> indent (unlines $ firstLine . skewPretty . nf <$> toList args)

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


-- Parsing and Printing --------------------------------------------------------

data AST
  = VAR Text
  | JET Text
  | APP AST AST
  | NAT Nat
 deriving (Eq, Ord, Generic, NFData)

data Bind = BJet Text Nat [Text] AST
          | BVal Text AST
  deriving (Eq, Ord, Generic, NFData)

data Prog = Prog [Bind] AST
  deriving (Eq, Ord, Generic, NFData)

instance R.ToRunic AST where
  toRunic = \case
    NAT l   -> R.Leaf $ tshow $ T l
    VAR v   -> R.Leaf $ v
    JET v   -> R.Leaf $ "$" <> v
    APP x y -> R.appRune (R.toRunic <$> appList x [y])
   where
    appList (APP x y) acc = appList x (y:acc)
    appList x acc         = x:acc

instance R.ToRunic Prog where
  toRunic (Prog [] top)    = R.toRunic top
  toRunic (Prog binds top) = R.Cor1 "|^" (R.toRunic top) (bindRune <$> binds)
   where
    paren = R.IFix "(" ")" . fmap R.Leaf

    bindRune (BVal nam bod) =
      ( R.Leaf nam
      , R.toRunic bod
      )

    bindRune (BJet nam tag arg bod) | utf8Atom nam==tag =
      ( paren (nam:arg)
      , R.toRunic bod
      )

    bindRune (BJet nam tag arg bod) =
      ( R.Leaf nam
      , R.RunC "~/"
          [ R.Leaf (tshow (T tag))
          , R.RunC "|="
              [ paren arg
              , R.toRunic bod
              ]
          ]
      )

instance Show AST where show = unpack . R.runicShowWide
instance Show Prog where show = unpack . R.runicShowWide

skewPretty :: Skew -> String
skewPretty = unpack . R.runicShow . expProg . L

progPretty :: Prog -> IO ()
progPretty = putStrLn . R.runicShow

progExp :: Prog -> Either Text Exp
progExp (Prog binds expr) = go [] expr
 where
  bindName :: Bind -> Text
  bindName (BJet n _ _ _) = n
  bindName (BVal n _)     = n

  go :: [Text] -> AST -> Either Text Exp
  go args = \case
    APP x y -> A <$> go args x <*> go args y
    NAT n   -> Right $ L $ NT n
    JET j -> do
      jv <- find ((==j) . bindName) binds & \case
        Nothing -> Left ("bad jet: " <> j)
        Just x -> Right x
      case jv of
        BJet _ t xs b -> do
          bexp <- go xs b
          let arg = fromIntegral $ length xs
          () <- if arg == 0
                then Left "Just must have >0 argument"
                else pure ()
          pure $ L $ funSkew (arg, t, bexp)
        BVal _ v -> go [] v
    VAR v -> do
      i <- case elemIndex v args of
             Nothing -> Left ("bad variable: " <> v)
             Just g  -> Right g
      pure $ V $ fromIntegral i

allVars :: [Text]
allVars = cs <> go 1
 where
  go = \(n::Int) -> fmap (<> tshow n) cs <> go (n+1)
  cs = singleton <$> "pqrstuvwxyz"

type Register = Nat -> Nat -> Exp -> IO Text

expProg :: Exp -> Prog
expProg expr = unsafePerformIO $ do
  jets <- newIORef (mempty :: Map Text Bind)

  let reg :: Register
      reg arg tag bod = do
        bodAst <- expAST reg bod

        let nams = (allVars !!) . fromIntegral <$> [0 .. arg-1]

        tbl <- readIORef jets

        let gensym :: Text -> Text
            gensym k =
              case lookup k tbl of
                Nothing -> k
                Just (BVal _ _) -> error "This shouldn't happen"
                Just (BJet _ t r b) ->
                  if (t,r,b) == (tag, nams, bodAst)
                  then k
                  else gensym (k <> "-")

        let fallback = "j" <> tshow tag
        let key  = gensym (either (const fallback) id (atomUtf8 tag))
        let bind = BJet key tag nams bodAst

        writeIORef jets (insertMap key bind tbl)

        pure key

  body <- expAST reg expr
  Prog <$> (toList <$> readIORef jets) <*> pure body

expAST :: Register -> Exp -> IO AST
expAST reg = go
 where
  go = \case
    V n     -> pure $ VAR (allVars !! fromIntegral n)
    L n     -> skewAST n
    A x y   -> APP <$> go x <*> go y

  skewAST (N l xs) = do
    a  <- leafAST l
    as <- traverse skewAST (reverse xs)
    pure (foldl' APP a as)

  leafAST (T t)                       = pure (NAT t)
  leafAST (J (getJet -> Jet n t _ x)) = JET <$> reg n t x


-- Parsing ---------------------------------------------------------------------

data Mode = Wide | Tall
  deriving (Eq, Ord, Show)

type Parser = StateT Mode (Parsec Void Text)

withLocalState ∷ Monad m => s → StateT s m a → StateT s m a
withLocalState val x = do { old <- get; put val; x <* put old }

inWideMode ∷ Parser a → Parser a
inWideMode = withLocalState Wide

grouped :: Text -> Text -> Text -> Parser a -> Parser [a]
grouped open delim close elmt = string open >> body
  where
    body = shut <|> (:) <$> elmt <*> rest
    rest = many (string delim *> elmt) <* shut
    shut = string close $> []

ace, pal, par ∷ Parser ()
ace = void (char ' ')
pal = void (char '(')
par = void (char ')')

comment :: Parser ()
comment = void (string "::" >> many (anySingleBut '\n') >> eol)

bulkSpace :: Parser ()
bulkSpace = void $ many (void spaceChar <|> comment)

gap ∷ Parser ()
gap = choice [ string " \n" >> bulkSpace
             , string "  "  >> bulkSpace
             , char '\n'    >> bulkSpace
             , comment      >> bulkSpace
             ]

parseRune ∷ Parser a → Parser a → Parser a
parseRune tall wide = get >>= \case
  Wide → wide
  Tall → tall <|> inWideMode wide

-- TODO - only in middle, support prime
alphaChar :: [Char]
alphaChar = ['a'..'z'] <> ['A'..'Z']

alpha ∷ Parser Char
alpha = oneOf alphaChar

symChar :: Parser Char
symChar = oneOf (['-'] ++ alphaChar ++ ['0'..'9'])

sym ∷ Parser Text
sym = (pack <$> ((:) <$> alpha <*> many symChar))
  <|> string "$"

runeJogging1 :: (a -> [(b, c)] -> r)
             -> Parser a -> Parser b -> Parser c -> Parser r
runeJogging1 node a b c = parseRune tall wide
 where
  tall = do
    x     <- gap *> a
    elems <- gap *> many (do { y <- b; gap; z <- c; gap; pure (y, z); })
    string "=="
    pure $ node x elems

  wide = do
    string "("
    x <- a
    elems <- wideBody <|> (char ')' $> [])
    pure $ node x elems

  wideBody = do
    hack <- grouped "; " ", " ")" (do { y <- b; ace; z <- c; pure (y,z) })
    case hack of
      [] -> fail "leave off '; ' for empty jogging segment"
      _  -> pure hack

fun :: Parser (Either AST (Nat, ([Text], AST)))
fun = try (Right <$> jet) <|> (Left <$> ast)
 where
  jet = string "~/" *> rune2 (,) tag lam

  lam = string "|=" *> rune2 (,) args ast

  args :: Parser [Text]
  args = grouped "(" " " ")" sym <|> (: []) <$> sym

  tag :: Parser Nat
  tag = atom <|> utf8Atom <$> sym

runeSwitch ∷ [(Text, Parser a)] → Parser a
runeSwitch = choice . fmap (\(s, p) → string s *> p)

appN ∷ [AST] → AST
appN = \case
  []     -> NAT 0
  [x]    -> x
  (x:xs) -> go x xs
 where
  go acc []     = acc
  go acc (x:xs) = go (APP acc x) xs

app3 :: AST -> AST -> AST -> AST
app3 x y z = appN [x, y, z]

app4 :: AST -> AST -> AST -> AST -> AST
app4 x y z p = appN [x, y, z, p]

cord ∷ Parser Text
cord = tik <|> cen
 where
  cen = char '%' *> sym
  tik = between (char '\'') (char '\'') $
          pack <$> many (label "cord char" (anySingleBut '\''))

atom ∷ Parser Nat
atom = do
  frst ← some digitChar
  rest ← many (char '.' *> some digitChar)
  guard True -- TODO Validate '.'s
  pure (Prelude.read $ concat $ frst:rest)

jetRef :: Parser Text
jetRef = char '$' *> sym

irregular :: Parser AST
irregular =
  inWideMode $ choice
    [ JET <$> jetRef
    , NAT <$> atom
    , NAT <$> (utf8Atom <$> cord)
    , VAR <$> sym
    , appN <$> grouped "(" " " ")" ast
    ]

ast :: Parser AST
ast = try rune <|> irregular

rune :: Parser AST
rune = runeSwitch
  [ ("%-", rune2 APP ast ast)
  , ("%+", rune3 app3 ast ast ast)
  , ("%^", rune4 app4 ast ast ast ast)
  , ("%*", runeN appN ast)
  , ("%.", rune2 (flip APP) ast ast)
  ]

rune2 ∷ (a→b→c) → Parser a → Parser b → Parser c
rune2 node x y = parseRune tall wide
  where tall = do gap; p←x; gap; q←y;      pure (node p q)
        wide = do pal; p←x; ace; q←y; par; pure (node p q)

rune3 ∷ (a→b→c→d) → Parser a → Parser b → Parser c → Parser d
rune3 node x y z = parseRune tall wide
  where tall = do gap; p←x; gap; q←y; gap; r←z;      pure (node p q r)
        wide = do pal; p←x; ace; q←y; ace; r←z; par; pure (node p q r)

rune4 ∷ (a→b→c→d→e) → Parser a → Parser b → Parser c → Parser d → Parser e
rune4 node x y z g = parseRune tall wide
  where tall = do gap; p←x; gap; q←y; gap; r←z; gap; s←g; pure (node p q r s)
        wide = do pal; p←x; ace; q←y; ace; r←z; ace; s←g; pure (node p q r s)

runeN ∷ ([a]→b) → Parser a → Parser b
runeN node elmt = node <$> parseRune tall wide
  where tall = gap >> elmts
                 where elmts   = term <|> elmtAnd
                       elmtAnd = do x ← elmt; gap; xs ← elmts; pure (x:xs)
                       term    = string "==" $> []
        wide = pal *> option [] elmts <* par
                 where elmts = (:) <$> elmt <*> many (ace >> elmt)

prog :: Parser Prog
prog = string "|^" *> runeJogging1 mkProg ast sym fun
 where
  mkProg :: AST -> [(Text, Either AST (Nat, ([Text], AST)))] -> Prog
  mkProg b bs = Prog (mkJet <$> bs) b

  mkJet :: (Text, Either AST (Nat, ([Text], AST))) -> Bind
  mkJet (n, Left expr)         = BVal n expr
  mkJet (n, Right (t, (args, body))) = BJet n t args body

whitespace ∷ Parser ()
whitespace = gap <|> ace

soonFile :: Parser Prog
soonFile = do
  option () whitespace
  p ← prog
  option () whitespace
  eof
  pure p

parseAST :: FilePath -> Text -> Either Text Prog
parseAST fp txt =
  runParser (evalStateT soonFile Tall) fp txt & \case
    Left  e → Left (pack $ errorBundlePretty e)
    Right x → pure x


-- REPL and Program Runner -----------------------------------------------------

repl :: IO ()
repl = error "TODO"

execSoon :: FilePath -> IO ()
execSoon fp = do
  txt <- decodeUtf8 <$> readFile fp
  parseAST fp txt & \case
    Left er -> putStrLn ("ERROR: " <> er)
    Right x -> progExp x & \case
      Left err -> error (unpack err)
      Right ex -> do
        skew <- evaluate $ force $ compile ex
        -- print ("inpu"::Text, skew)
        resu <- evaluate $ force $ nf skew
        -- print ("resu"::Text, resu)
        res <- evaluate (expProg $ L resu)
        progPretty res

parseSoonTest ∷ Text → IO ()
parseSoonTest = parseTest (evalStateT soonFile Tall)


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
