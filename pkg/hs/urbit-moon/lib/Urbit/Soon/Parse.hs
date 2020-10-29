{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Soon.Parse
  ( Prog(..)
  , Bind(..)
  , AST(..)
  , progExp
  , expProg
  , progPretty
  , execSoon
  , soonFile
  , parseAST
  , parseSoonTest
  , skewPretty
  ) where

import ClassyPrelude        hiding (bracket, many, some, try)
import Numeric.Natural
import Text.Megaparsec
import Text.Megaparsec.Char
import Urbit.Soon.Exp

import Control.Monad.State.Lazy (StateT, evalStateT, get, put)
import Data.Function            ((&))
import Data.List                (elemIndex, (!!))
import Data.Void                (Void)
import System.IO.Unsafe         (unsafePerformIO)
import Urbit.Atom               (atomUtf8, utf8Atom)

import qualified Prelude
import qualified Urbit.Runic as R

type Nat = Natural


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
        let jkey = gensym (either (const fallback) id (atomUtf8 tag))
        let bind = BJet jkey tag nams bodAst

        writeIORef jets (insertMap jkey bind tbl)

        pure jkey

  body <- expAST reg expr
  Prog <$> (toList <$> readIORef jets) <*> pure body


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
runeJogging1 node a b c = do
  gap
  x     <- a
  gap

  elems <- many $ do
    string "++"
    gap
    y <- b
    gap
    z <- c
    gap
    pure (y, z)

  string "=="
  pure $ node x elems

argList :: Parser [Text]
argList = grouped "(" " " ")" sym <|> (: []) <$> sym

fun :: Parser (Either AST (Nat, ([Text], AST)))
fun = try (Right <$> jet) <|> (Left <$> ast)
 where
  jet = string "~/" *> rune2 (,) tag lam

  lam = string "|=" *> rune2 (,) argList ast

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

key :: Parser (Either Text [Text])
key = fmap Left sym <|> fmap Right argList

prog :: Parser Prog
prog = join (string "|^" *> runeJogging1 mkProg ast key fun)
 where
  mkProg :: AST -> [(Either Text [Text], Either AST (Nat, ([Text], AST)))] -> Parser Prog
  mkProg b bs = do
    p <- traverse mkJet bs
    pure (Prog p b)

  mkJet :: (Either Text [Text], Either AST (Nat, ([Text], AST))) -> Parser Bind
  mkJet = \case
    (Right [], _) ->
      fail $ "`++  ()  body` makes no sense. What do you mean?"
    (Right [_], _) ->
      fail $ "`++  (x)  asdf` makes no sense. Just write `++  x  y`."
    (Right _, Right _) ->
      fail $ "`++  (f arg ...)  exp` already implies jet hint"
    (Right (f:args), Left body) ->
      pure $ BJet f (utf8Atom f) args body
    (Left n, Left expr) ->
      pure $ BVal n expr
    (Left n, Right (t, (args, body))) ->
      pure $ BJet n t args body

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

execSoon :: FilePath -> IO ()
execSoon fp = do
  txt <- decodeUtf8 <$> readFile fp
  parseAST fp txt & \case
    Left er -> putStrLn ("ERROR: " <> er)
    Right x -> progExp x & \case
      Left err -> error (unpack err)
      Right ex -> do
        skew <- evaluate $ force $ compile ex
        resu <- evaluate $ force $ nf skew
        evaluate (expProg $ L resu) >>= progPretty

parseSoonTest ∷ Text → IO ()
parseSoonTest = parseTest (evalStateT soonFile Tall)


-- Testing ---------------------------------------------------------------------

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
