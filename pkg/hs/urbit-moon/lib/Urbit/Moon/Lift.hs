{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Moon.Lift
  ( lamWrap
  , lamUnwrap

  , moonProg
  , progMoon

  , liftExp
  , lamLift

  , progEx
  ) where

import Bound
import ClassyPrelude
import Control.Lens
import Data.Void
import Urbit.Moon.CAF

import Control.Monad.State

import Urbit.Moon.Exp

import Bound.Var       (unvar)
import Data.List       (findIndex)
import Urbit.Moon.AST  ()
import Urbit.Moon.Atom (Atom(NAT))
import Urbit.Moon.Prim (Op(NAT_ADD))
import Urbit.Moon.Bind ()


-- Types -----------------------------------------------------------------------

-- data Prog = Prog [(Text, Int, Exp Int)]


--------------------------------------------------------------------------------

liftExp :: ∀a. Eq a => Exp a -> (Exp Int, [a])
liftExp = flip runState [] . traverse \x -> do
  xs <- get
  case findIndex (x ==) xs of
    Nothing -> put (xs <> [x]) $> length xs
    Just i  -> pure i

--------------------------------------------------------------------------------

lamWrap :: Int -> Exp Int -> Exp a
lamWrap =
  \numArgs topExpr -> fmap badRef (go numArgs topExpr)
 where
  badRef :: ∀a. Int -> a
  badRef i = error ("Invalid reference index: " <> show i)

  decRef :: Int -> Int -> Var () Int
  decRef n i = compare i (n-1) & \case
    EQ -> B ()
    LT -> F i
    GT -> badRef i

  go :: Int -> Exp Int -> Exp Int
  go 0 = id
  go n = go (n-1) . Lam . Scope . fmap (unvar B (F . Var) . decRef n)

lamUnwrap :: Int -> Exp Int -> (Int, Exp Int)
lamUnwrap numFree = go id numFree
 where
  go :: ∀a. (a -> Int) -> Int -> Exp a -> (Int, Exp Int)
  go f n = \case
    Lam b -> go (unvar (const n) f) (n+1) (fromScope b)
    x       -> (n, fmap f x)

moonProg :: Exp Void -> Prog
moonProg = \x -> Prog [("main", 1, go "main" (absurd <$> x))]
 where
  go :: Eq a => Text -> Exp a -> CAF a
  go c = \case
    Var v     -> CVar v
    App x y   -> CApp (go c x) (go c y)
    Con s n x -> CCon s n (go c <$> x)
    Pat x ps  -> CPat (go c x) (over _2 (toScope . go c . fromScope) <$> ps)
    Seq x y   -> CSeq (go c x) (go c y)
    Lit l     -> CLit l
    Cas x cs  -> CCas (go c x) (over _2 (go c) <$> cs)
    Vec xs    -> CVec (go c <$> xs)
    Opr o     -> COpr (go c <$> o)
    Lam b     -> cafApp (cafNam c nArg $ go c bod) (CVar <$> fre)
                    where (bod, fre, nArg) = lamLift (Lam b)
    Let vs b -> error "TODO: moonToCLet" vs b


progEx :: Prog
progEx = Prog
  [ ("main", 0, CGlo "asdf")
  , ("nini", 0, nat 99)
  , ("asdf", 3, COpr (NAT_ADD (CGlo "nini") (CVar 0)))
  , ("f", 1, CGlo "g" `CApp` CVar 0)
  , ("g", 1, CGlo "f" `CApp` CVar 0)
  ]
 where
  nat = CLit . NAT

{-
getSym :: Prog -> Text -> (Int, CAF Int)
getSym (Prog ps) n =
  cvt $ fromMaybe bad $ find ((==n) . view _1) ps
 where
  bad = error ("undefined symbol: " <> unpack n)
  cvt = \(_,y,z) -> (y,z)
-}

{-
    - TODO Merge `CGlo` and `CVar` and use a traversal for this.

    - TODO This doesn't handle mutual recursion.

      - Instead of taking an Prog, take a function (a -> Exp a)
        which resolves to a reference to a big top-level `Let`.
-}

{-
cafExp :: Prog -> CAF a -> Exp a
cafExp prog = go
 where
  go :: ∀a. CAF a -> Exp a
  go = \case
    CVar v     -> Var v
    CGlo sym   -> fmap absurd $ uncurry lamWrap $ over _2 go $ getSym prog sym
    CApp x y   -> App (go x) (go y)
    CCon s n x -> Con s n (go <$> x)
    CFor x     -> For (go x)
    CDel x     -> Del (go x)
    CLit l     -> Lit l
    CVec xs    -> Vec (go <$> xs)
    COpr o     -> Opr (go <$> o)
    CPat x ps  -> Pat (go x) (over _2 (toScope . go . fromScope) <$> ps)
    CCas x cs  -> Cas (go x) (over _2 go <$> cs)
-}

{-
  TODO Merge `CGlo` and `CVar` and use a traversal for this.
-}
cafExp :: (Text -> var) -> CAF var -> Exp var
cafExp = go
 where
  go :: ∀a. (Text -> a) -> CAF a -> Exp a
  go f = \case
    CVar v     -> Var v
    CGlo sym   -> Var (f sym)
    CApp x y   -> App (go f x) (go f y)
    CCon s n x -> Con s n (go f <$> x)
    CSeq x y   -> Seq (go f x) (go f y)
    CLit l     -> Lit l
    CVec xs    -> Vec (go f <$> xs)
    COpr o     -> Opr (go f <$> o)
    CPat x ps  -> Pat (go f x) $ ps <&> over _2 (throughScope $ go $ F . f)
    CCas x cs  -> Cas (go f x) $ cs <&> over _2 (go f)

  throughScope :: (Monad m, Monad n)
               => (m (Var v a) -> n (Var v b))
               -> Scope v m a
               -> Scope v n b
  throughScope f = toScope . f . fromScope

{-
  This should be straightforward.

  Convert the program into a big `Let` expression whose body is a
  reference to the global named "main".

  Use this: `cafExp :: (Text -> var) -> CAF var -> Moon var`

  - `var` will be `Var Int Int`, where:

    - `F i` refers to an element of the binding.
    - `B i` refers to an an argument to the lambda.

  - The first arguent to that fn is just a function that finds the index
    of string in the Prog list.
-}
progMoon :: Prog -> Exp Void
progMoon (Prog ps) =
  Let (f <$> ps) mainRef
 where
  gloIdx :: Text -> Int
  gloIdx sym = fromMaybe (error $ "undefined symbol: " <> unpack sym)
              $ findIndex ((== sym) . view _1) ps

  mainRef = Scope $ Var $ B $ gloIdx "main"

  f :: (Text, Int, CAF Int) -> Scope Int Exp Void
  f = toScope . fmap B . g

  -- TODO This is broken because it confliats the Ints that are references
  -- into the let and the Ints that are references into the lambda's
  -- arguments.
  g :: (Text, Int, CAF Int) -> Exp Int
  g (_nm, _args, body) = cafExp gloIdx body
  -- g (_nm, args, body) = lamWrap args (cafExp gloIdx body)
  -- lamWrap :: Int -> Exp Int -> Exp a

{-
  This is basically the right shape except that it needs to be monadic.

  This should create a new global function and return a reference to it.
-}
cafNam :: Text -> Int -> CAF Int -> CAF a
cafNam ctxName numArgs _body = CGlo (ctxName <> "-" <> tshow numArgs)

cafApp :: CAF a -> [CAF a] -> CAF a
cafApp x []     = x
cafApp x (r:rs) = cafApp (x `CApp` r) rs

lamLift :: Eq a => Exp a -> (Exp Int, [a], Int)
lamLift expr = (body, freeRefs, numArgs)
 where
  (liftedExpr, freeRefs) = liftExp expr
  (numArgs, body) = lamUnwrap (length freeRefs) liftedExpr

{-

++  (adder x)  [<y (add 2 (add x y))> <y (add 3 (add x y))>]

++  (adder-lift1 x y)  (add 2 (add x y))
++  (adder-lift2 x y)  (add 3 (add x y))
++  (adder x)          [(adder-lift1 x) (adder-lift2 x)]

- Convert a string of lambdas into a single lambda of n-args: (Exp Int)

- Every unique free variable reference that we see becomes a new argument
  (at the head of the list).

- Variable references should be (Either Int Int)

-}
