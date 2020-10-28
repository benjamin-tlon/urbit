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

mkRec :: Eq a => [(a,Exp a)] -> Exp a -> Exp a
mkRec [] b = b
mkRec bs b = Rec (bs <&> abstr . snd) (abstr b)
 where
  abstr = abstract (`elemIndex` map (view _1) bs)

mkLet :: Eq a => [(a, Exp a)] -> Exp a -> Exp a
mkLet [] b = b
mkLet bs b = Let (snd <$> bs) (abstr b)
 where
  abstr = abstract (`elemIndex` map fst bs)

recBinds :: [Bind] -> AST -> Exp Text
recBinds bs x = mkRec (go <$> bs) (bind x)
 where
  go (n, rs, b) = (n, bind (lam rs b))

  lam []     b = b
  lam (n:ns) b = LAM n (lam ns b)

letBinds :: [(Text, AST)] -> AST -> Exp Text
letBinds bs x = mkLet (over _2 bind <$> bs) (bind x)



-- Bind Names ------------------------------------------------------------------

bind :: AST -> Exp Text
bind = go
 where
  go = \case
    VAR t              -> Var t
    APP x y            -> App (go x) (go y)
    LAM v b            -> Lam (abstract1 v (go b))
    LET bs x           -> letBinds bs x
    REC bs x           -> recBinds bs x
    CON ns n xs        -> Con ns n (go <$> xs)
    PAT x pats         -> Pat (go x) $ pats <&> \(vs, b) ->
                            ( fromIntegral $ length vs
                            , abstract (`elemIndex` vs) $ bind b
                            )
    OPR o              -> Opr (go <$> o)
    LIT l              -> Lit l
    SEQ x y            -> Seq (go x) (go y)
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
    Rec ls b    -> REC (goBinds ls) (recurInN b)
    Let ls b    -> LET (goBiVls ls) (recurInN b)
    Con ns n xs -> CON ns n (recur <$> xs)
    Pat x bs    -> PAT (recur x) (goPat <$> bs)
    Seq x y     -> SEQ (recur x) (recur y)
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

    goBiVls :: [Exp a] -> [(Text, AST)]
    goBiVls = zipWith (\i b -> (v <> tshow @Int i, recur b)) [0..]

    goBinds :: [Scope Int Exp a] -> [Bind]
    goBinds = zipWith (\i b -> (v <> tshow @Int i, [], recurInN b)) [0..]
