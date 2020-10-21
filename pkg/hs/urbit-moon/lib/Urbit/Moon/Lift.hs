{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Moon.Lift
  ( lamWrap
  , lamUnwrap

  , expProg
  , progExp

  , liftExp
  , liftLam

  , progEx
  ) where

import Bound
import ClassyPrelude
import Control.Lens
import Control.Monad.State
import Data.Void
import Urbit.Moon.CAF
import Urbit.Moon.Exp

import Bound.Var       (unvar)
import Data.List       (findIndex, (!!))
import Urbit.Moon.AST  ()
import Urbit.Moon.Atom (Atom(NAT))
import Urbit.Moon.Bind (unbind)
import Urbit.Moon.Bind ()
import Urbit.Moon.Prim (Op(NAT_ADD))


-- Turn Free Variables into Arg List -------------------------------------------

liftExp :: ∀a. Eq a => Exp a -> (Exp Int, [a])
liftExp = flip runState [] . traverse \x -> do
  xs <- get
  case findIndex (x ==) xs of
    Nothing -> put (xs <> [x]) $> length xs
    Just i  -> pure i


-- Convert CAF into Lambda Calculus (Trivial) ----------------------------------

cafExp :: CAF a -> Exp a
cafExp = \case
  CVar v     -> Var v
  CApp x y   -> App (cafExp x) (cafExp y)
--CCon s n x -> Con s n (cafExp <$> x)
  CSeq x y   -> Seq (cafExp x) (cafExp y)
  CLit l     -> Lit l
  CVec xs    -> Vec (cafExp <$> xs)
  COpr o     -> Opr (cafExp <$> o)
--CPat x ps  -> Pat (cafExp x) $ ps <&> over _2 (toScope . cafExp . fromScope)
--CCas x cs  -> Cas (cafExp x) $ cs <&> over _2 cafExp


-- Convert CAF Argument Numbers into Lambda Expressions -------------------

lamWrap :: Int -> Exp (Var Int a) -> Exp a
lamWrap = \args top -> unvar badRef id <$> go args top
 where
  badRef :: Int -> void
  badRef = error . ("Invalid reference index: " <>) . show

  decRef :: Int -> Int -> Var () Int
  decRef n i = compare i (n-1) & \case
    EQ -> B ()
    LT -> F i
    GT -> badRef i

  go :: Int -> Exp (Var Int a) -> Exp (Var Int a)
  go 0 = id
  go n = go (n-1) . Lam . Scope . fmap \case
    B i -> Var . B <$> decRef n i
    F x -> F $ Var $ F x


-- Strip off a Sequence of Lambdas, Replacing Argument Refs with Ints ----------

lamUnwrap :: Exp a -> (Int, Exp (Var Int a))
lamUnwrap = go 0 F
 where
  go :: Int -> (v -> Var Int a) -> Exp v -> (Int, Exp (Var Int a))
  go n f = \case
    Lam b -> go (n+1) (unvar (B . const n) f) (fromScope b)
    x     -> (n, f <$> x)


-- Converts CAFs into an Expression --------------------------------------------

progExp :: Prog -> Exp Void
progExp (Prog ps) =
  Let (Scope . bindExp <$> ps)
      (Scope $ Var $ B $ gloIdx "main")
 where
  gloIdx :: Text -> Int
  gloIdx sym = fromMaybe (undef sym) $ findIndex ((== sym) . view _1) ps

  undef :: Text -> void
  undef = error . ("undefined symbol: " <>) . unpack

  bindExp :: ∀a b. (a, Int, CAF (Var Int Text)) -> Exp (Var Int b)
  bindExp (_,r,e) = B . gloIdx <$> lamWrap r (cafExp e)


-- Lambda Lifting --------------------------------------------------------------

liftLam :: Eq a => Exp a -> (Exp Int, [a], Int)
liftLam expr = (unvar (+ numFree) id <$> body, freeRefs, numArgs)
 where
  (liftedExpr, freeRefs) = liftExp expr
  numFree = length freeRefs
  (numArgs, body) = lamUnwrap liftedExpr


-- Convert Expressions into CAFs using Lambda Lifting --------------------------

conExp :: [Int] -> Int -> [Exp a] -> Exp a
conExp cs i xs =
  if length xs == (cs !! i)
  then expApp (lamWrap conArgs (B <$> lamBody)) xs
  else error
     $ ("Undersaturated Constructor: " <>)
     $ show $ unbind $ Con cs i
     $ xs <&> const (Var "_")
 where
  lamBody = expApp kalBack argExps
  argExps = Var <$> [0 .. valArgs-1]
  kalBack = Var (valArgs + i)
  conArgs = valArgs + patArgs
  patArgs = length cs
  valArgs = cs !! i

patExp :: Exp a -> [(Int, Scope Int Exp a)] -> Exp a
patExp x ps = expApp x (ps <&> \(r,b) -> lamWrap r (fromScope b))

casExp :: Exp a -> [(Maybe Atom, Exp a)] -> Exp a
casExp = error "TODO: switch on atoms"

expProg :: Exp Void -> Prog
expProg top = Prog [("main", 1, go "main" id (absurd <$> top))]
 where
  go :: (Show a, Eq a)
     => Text -> (Text -> a) -> Exp (Var Int a) -> CAF (Var Int a)
  go c f = \case
    Var v     -> CVar v
    App x y   -> CApp (go c f x) (go c f y)
    Con s n x -> go c f (conExp s n x)
    Pat x ps  -> go c f (patExp x ps)
    Seq x y   -> CSeq (go c f x) (go c f y)
    Lit l     -> CLit l
    Cas x cs  -> go c f (casExp x cs)
    Vec xs    -> CVec (go c f <$> xs)
    Opr o     -> COpr (go c f <$> o)
    Let vs b  -> error "TODO: LET" vs b
    Lam b     -> cafApp
                   (CVar $ F $ f $ cafNam c arg $ go c id $ fmap B bod)
                   (CVar <$> fre)
     where
      (bod, fre, arg) = liftLam (Lam b)

{-
  Let's say that we just blindly lift all bindings. Is that actually easy?

  Well, actually: First, we should convert each let binding to a CAF,
  this will remove all lambdas and move all bindings to the top
  level. Then, each binding will just be a series of applications,
  possibly referencing other bindings.

  |^  (x y z)
  ++  x  (x y z)
  ++  y  (x y z)
  ++  z  (x y z)
  ==

  If these bindings were acyclic, then it's easy:

  - Extract all bindings that don't reference other bindings into a
    CAS-CON expression.
  - Repeat until there are no bindings.

  If the bindings are simple recursion, what do we do? We lift it and
  make it reference itself.

  =/  x  (x x)  x
    BECOMES
      ++  main-x  (main-x main-x)
      ++  main  main-x

  What if the bindings are mutual recursion? What do we do? We lift both
  and make them reference each-other.

  |^  x
  ++  x  y
  ++  y  x
  ==
    BECOMES
      ++  main-x  main-y
      ++  main-y  main-x
      ++  main    main-x

  What does that look like if there are things that need to be closed over?

  THIS:

      ++  (main arg)
        |^  x
        ++  x  #[y arg]
        ++  y  #[x arg]

  BECOMES

      ++  (main-x arg)  #[(main-y arg) arg]
      ++  (main-y arg)  #[(main-x arg) arg]
      ++  (main arg)
        |^  x
        x   (main-x arg)
        y   (main-y arg)

  BECOMES

      ++  (main-x arg)  #[(main-y arg) arg]
      ++  (main-y arg)  #[(main-x arg) arg]
      ++  (main arg)
        ?-  [(main-x arg) (main-y arg)]
        ++  [x y]  x

  SIMPLIFIES TO

      infinite loop

  So the algoithm is:
    - If are no bindings, return the expression.
    - If there are bindings that don't reference other bindings:
      - extract that binding into a CON-CAS expression.
      - Repeat algorithm.
    - If we got here, the first binding will be part of a cycle.
      - Find all of the bindings in that cycle
      - Lift them all.
      - Replace all the bindings in the cycle with a reference to the
        new super combinator.
      - These bindings will no longer be a cycle.
      - goto top.

  Later, we should simplify. Hopefully that removes all of the garbage
  top-level definitions that we're creating.

  Alright, how about mutual recursion? Should we handle that here?

  I've settled on the following approach to encoding mutual-recursion,
  since I believe I can unwrap it in the optimizer:

  THIS:

      ++  (main arg)
        |^  (odd arg)
        ++  (odd n)
          ?-  (NAT_DEC n)
          ++  []   #f
          ++  [m]  (even m)
        ++  (even n)
          ?-  (NAT_DEC n)
          ++  []   #t
          ++  [m]  (odd m)

  BECOMES

      ++  (main-odd even n)  ?-((NAT_DEC n); [] #f, [m] (even m))
      ++  (main-even odd n)  ?-((NAT_DEC n); [] #t, [m] (odd m))
      ++  (main arg)
        |^  (odd arg)
        ++  odd   (main-odd even)
        ++  even  (main-even odd)

  BECOMES

      ++  (main-odd even n)  ?-((NAT_DEC n); [] #f, [m] (even m))
      ++  (main-even odd n)  ?-((NAT_DEC n); [] #t, [m] (odd m))
      ++  main-odd-cycle   (main-odd main-even-cycle)
      ++  main-even-cycle  (main-even main-odd-cycle)
      ++  (main arg)
        ?-  [main-odd-cycle main-even-cycle]
        ++  [even odd]
          (odd arg)

  SIMPLIFIES TO

      ++  (main-odd even n)  ?-((NAT_DEC n); [] #f, [m] (even m))
      ++  (main-even odd n)  ?-((NAT_DEC n); [] #t, [m] (odd m))
      ++  main-odd-cycle     (main-odd main-even-cycle)
      ++  main-even-cycle    (main-even main-odd-cycle)
      ++  (main arg)         (main-odd-cycle arg)

  DECYCLES TO

      ++  (main-odd even n)       ?-((NAT_DEC n); [] #f, [m] (even m))
      ++  (main-even odd n)       ?-((NAT_DEC n); [] #t, [m] (odd m))
      ++  (main-odd-cycle ev n)   (main-odd (ev main-odd-cycle) n)
      ++  (main-even-cycle od n)  (main-even (od main-even-cycle) n)
      ++  (main-odd-op n)         (main-odd-cycle main-even-cycle n)
      ++  (main-even-op n)        (main-even-cycle main-odd-cycle n)
      ++  (main arg)              (main-odd-op arg)

  EVALUATES TO

      (main 2)
      (main-odd-op 2)
      (main-odd-cycle main-even-cycle 2)
      (main-even (main-even-cycle main-odd-cycle) 2)
      ?-((NAT_DEC 2); [] #f, [m] (main-even-cycle main-odd-cycle m))
      (main-even-cycle main-odd-cycle 1)
      (main-even (main-odd-cycle main-even-cycle) 1)
      ?-((NAT_DEC 1); [] #t, [m] (main-odd-cycle main-even-cycle m))
      (main-odd-cycle main-even-cycle 1)
      ?-((NAT_DEC 0); [] #f, [m] (main-even-cycle main-odd-cycle m))
      #f

  So, what's happening here? How does this encoding work?

  `main-even-cycle` is simply recursive: recursion is encoded with
  `fix`.

  `main-even-cycle` takes a callback function, and that callback
  function needs to know how to call the even function, but is
  prepared to give the even function it's told about a reference
  to itself.

  So, it works.

  But, can the runtime system see through this? Is this actually
  better than having a single loop that pattern matches on an ADT
  of all of the posibilities?

  ------------------------------------------------------------------------------

  I don't know. What would that look like? To compare?

      ++  (main-odd evn n)  ?-((NAT_DEC n); [] #f, [m] (evn m))
      ++  (main-evn odd n)  ?-((NAT_DEC n); [] #t, [m] (odd m))

      ++  (main-evn-odd x)
        ?-  x
        ++  [arg]  (main-evn <y (main-evn-odd %([1 1] 1 [y]))> arg)
        ++  [arg]  (main-odd <y (main-evn-odd %([1 1] 0 [y]))> arg)

      ++  (main-evn-op n)  (main-evn-odd #([1 1] 1 [n]))
      ++  (main-odd-op n)  (main-evn-odd #([1 1] 0 [n]))

  Could we encode this pattern in general, and jet it?

      type Loop_1_1 = (a → (a → c) → (b → c) → c)
                    → (b → (a → c) → (b → c) → c)
                    → Either a b
                    → c

      loop_1_1 :: Loop_1_1
      loop_1_1 f g (Lef x) = f (loop_1_1 f g . Rit) x
      loop_1_1 f g (Rit x) = g (loop_1_1 f g . Lef) x

  Yeah, I think that can be generalized!

      ++  (loop-1-1 f g b)
        |^  ?-  b
            ++  [l]  (f kalF calG l)
            ++  [r]  (g kalF kalG r)
        ++  (kalF x)  (loop-1-1 f g %([1 1] 0 [x]))
        ++  (kalG x)  (loop-1-1 f g %([1 1] 1 [x]))

  This is the core pattern, basically.

  ----------------------------------------------------------------------------

  - Simplest case of let: One binding, not recursive.

    `LET [x] y` becomes `CAS (CON [1] 0 [x]) x`

  - Next most simple case: Series of bindings that don't reference
    other bindings:

    `LET [x y z] b` becomes `CAS (CON [3] 0 [x y z]) b`

  - Simpliest recursive case:

      |^  (x |=(x x))
      ++  x  |=(y (y x))
      ==

  BECOMES

      ++  foo-x  |=(y (y foo-x))
      ++  foo  (foo-x I)

  So, there's no ordering to the thing, so we can't interate through
  the bindings.

    - first:
      - Find a binding that doesn't reference any other bindings.
        - If no such binding exists, goto second
      - Strip that off and replace it with pattern matching on a box.
      - Replace references to that bindings with references to the
        new variables.
      - goto first;
    - second:
      - Find a binding that only references itself.
        - If no such binding exists, goto third.
      - Create a CAF for the body of that reference.
      - Replace the body of the binding with an invokation of that CAF.
      - goto first
    - third:
      - Note that we are in a situation where all bindings are either
        mutually recursive or reference mutually recursive bindings.
      - Find a cycle of references that doesn't reference any bindings
        outside of that binding.
        - If no such cycle exists:
          - Done? TODO
      - What to do with that cycle? TODO
        outside of that binding.

      - What if we just do something really dumb instead?

  Take every binding and create a new CAF for it. They are allowed
  to be cyclic.

      ++  (ex arg)
      |^  foo
      ++  wut  arg
      ++  foo  (bar wut)
      ++  bar  (zaz wut)
      ++  zaz  wut
      ==

  Becomes

      ++  (ex-wut arg)  arg
      ++  (ex-foo arg)  ((ex-bar arg) (ex-wut arg))
      ++  (ex-bar arg)  ((ex-zaz arg) (ex-wut arg))
      ++  (ex-zaz arg)  (ex-wut arg)
      ++  (ex arg)  (ex-foo arg)

  Which simplifies down to:

      ++  (ex arg)  (arg arg arg)
-}

{-
  At the start, there are no references to CAFs.
  As we recurse down:
  We will see the first lambda
  All free variables in that lambda should become arguments.
  Now, we will have a lambda expression without free variables.
  However, that expression is still not a CAF.
  We must recur down into that expression, and do more lifting.
  After this transformation, there will be global references.
  So variables will have to have the type: Var Int Text
  The return value of expExp must have this shape.
-}

{-
  This is basically the right shape except that it needs to be monadic.

  This should create a new global function and return a reference to it.
-}
cafNam :: Text -> Int -> CAF (Var Int Text) -> Text
cafNam ctxName numArgs _body = ctxName <> "-" <> tshow numArgs

cafApp :: CAF a -> [CAF a] -> CAF a
cafApp x []     = x
cafApp x (r:rs) = cafApp (x `CApp` r) rs

expApp :: Exp a -> [Exp a] -> Exp a
expApp x []     = x
expApp x (r:rs) = expApp (x `App` r) rs


-- Examples for Testing --------------------------------------------------------

progEx :: Prog
progEx = Prog
  [ ("main", 0, glo "fdsa")
  , ("nini", 0, nat 99)
  , ("fdsa", 3, COpr (NAT_ADD (glo "nini") (arg 0)))
  , ("f", 1, glo "g" `CApp` arg 0)
  , ("g", 1, glo "f" `CApp` arg 0)
  ]
 where
  nat = CLit . NAT
  arg = CVar . B
  glo = CVar . F
