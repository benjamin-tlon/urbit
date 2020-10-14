{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Moon.Bind (bind, unbind) where

import Bound
import Bound.Var
import ClassyPrelude
import Urbit.Moon.AST
import Urbit.Moon.Exp

import Control.Lens (view, over, _1, _2)
import Data.List    (elemIndex, (!!))


-- Types -----------------------------------------------------------------------

type Bind = (Text, [Text], AST)


-- Utils -----------------------------------------------------------------------

mkLet :: Eq a => [(a,Exp a)] -> Exp a -> Exp a
mkLet [] b = b
mkLet bs b = Let (bs <&> abstr . snd) (abstr b)
 where
  abstr = abstract (`elemIndex` map (view _1) bs)

letBinds :: [Bind] -> AST -> Exp Text
letBinds bs x = mkLet (go <$> bs) (bind x)
 where
  go (n, rs, b) = (n, bind (lam rs b))

  lam []     b = b
  lam (n:ns) b = LAM n (lam ns b)


-- Bind Names ------------------------------------------------------------------

bind :: AST -> Exp Text
bind = go
 where
  go = \case
    VAR t              -> Var t
    APP x y            -> App (go x) (go y)
    LAM v b            -> Lam (abstract1 v (go b))
    LET bs x           -> letBinds bs x
    CON ns n xs        -> Con ns n (go <$> xs)
    PAT x pats         -> Pat (go x) $ pats <&> \(vs, b) ->
                            ( fromIntegral $ length vs
                            , abstract (`elemIndex` vs) $ bind b
                            )
    OPR o              -> Opr (go <$> o)
    LIT l              -> Lit l
    DEL x              -> Del (go x)
    FOR x              -> For (go x)
    CAS x cs           -> Cas (go x) (over _2 go <$> cs)
    VEC xs             -> Vec (go <$> xs)


-- Reconstruct AST -------------------------------------------------------------

allVars :: [Text]
allVars = ((singleton <$> ['a'..'z']) <> gogo 0)
 where
  gogo :: Int -> [Text]
  gogo n = (map (((allVars !! n) <>) . singleton) ['a'..'z']) <> gogo (succ n)

unbind :: Exp Text -> AST
unbind = go allVars id
 where
  go :: ∀a. [Text] -> (a -> Text) -> Exp a -> AST
  go vars f = \case
    Lam b       -> LAM v (recurIn1 b)
    Var x       -> VAR (f x)
    App x y     -> APP (recur x) (recur y)
    Let ls b    -> LET (goBinds ls) (recurInN b)
    Con ns n xs -> CON ns n (recur <$> xs)
    Pat x bs    -> PAT (recur x) (goPat <$> bs)
    Del x       -> DEL (recur x)
    For x       -> FOR (recur x)
    Cas x cs    -> CAS (recur x) (over _2 recur <$> cs)
    Opr o       -> OPR (recur <$> o)
    Lit l       -> LIT l
    Vec x       -> VEC (recur <$> x)
   where
    v:vs = vars

    recur :: Exp a -> AST
    recur = go vars f

    recurIn1 :: Scope () Exp a -> AST
    recurIn1 = go vs (unvar (const v) f) . fromScope

    recurInN :: Scope Int Exp a -> AST
    recurInN = go vs (unvar ((v <>) . tshow) f) . fromScope

    goPat :: (Int, Scope Int Exp a) -> ([Text], AST)
    goPat (n, sc) = (,) (mappend v . tshow @Int <$> [0 .. n-1])
                        (recurInN sc)

    goBinds :: [Scope Int Exp a] -> [Bind]
    goBinds = zipWith (\i b -> (v <> tshow @Int i, [], recurInN b)) [0..]
