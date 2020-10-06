{-# OPTIONS_GHC -Wall #-}

module Urbit.Moon.Bind (bind, unbind) where

import Bound
import ClassyPrelude
import Urbit.Moon.AST
import Urbit.Moon.Exp

import Data.List (elemIndex, (!!))


-- Utils -----------------------------------------------------------------------

let_ :: Eq a => [(a,Exp a)] -> Exp a -> Exp a
let_ [] b = b
let_ bs b = Let (map (abstr . snd) bs) (abstr b)
  where abstr = abstract (`elemIndex` map fst bs)

letBinds :: [Bind] -> AST -> Exp Text
letBinds bs ast = let_ (bs <&> \(Bind n x) -> (n, bind x)) (bind ast)

bindPat :: Pat -> (Nat, Scope Int Exp Text)
bindPat (MkPat vs x) =
  (,) (fromIntegral $ length vs)
      (abstract (`elemIndex` vs) $ bind x)

allVars :: [Text]
allVars = ((singleton <$> ['a'..'z']) <> go 2)
 where
  go :: Integer -> [Text]
  go n = (map (pack . (: show n)) ['a'..'z']) <> go (succ n)


-- Bind Names ------------------------------------------------------------------

bind :: AST -> Exp Text
bind = go
 where
  go = \case
    VAR t              -> Var t
    APP x y            -> App (go x) (go y)
    SEQ x y            -> Seq (go x) (go y)
    LAM v b            -> Lam (abstract1 v (go b))
    LET bs x           -> letBinds bs x
    CON (MkCon l xs r) -> Con l (go <$> xs) r
    PAT x pats         -> Pat (go x) (bindPat <$> pats)
    OPR o              -> Opr (go <$> o)


-- Reconstruct AST -------------------------------------------------------------

unbind :: Exp Text -> AST
unbind = go allVars id
 where
  go :: [Text] -> (a -> Text) -> Exp a -> AST
  go vars f = \case
    Lam b      -> LAM v $ go vs f' $ fromScope b
    Var x      -> VAR (f x)
    App x y    -> APP (go vars f x) (go vars f y)
    Seq x y    -> SEQ (go vars f x) (go vars f y)
    Let x b    -> LET (unbindBinds x) $ go (vsN (length x)) fi $ fromScope b
    Con l xs r -> CON (MkCon l (go vars f <$> xs) r)
    Pat x bs   -> PAT (go vars f x) (unbindPat <$> bs)
    Opr o      -> OPR (go vars f <$> o)

   where
    unbindPat (n, sc) =
      let ni = fromIntegral n
      in MkPat (take ni vars)
               (go (vsN ni) fi (fromScope sc))

    unbindBind i = Bind (vars !! i) . go vars fi . fromScope
    unbindBinds  = zipWith unbindBind [0..]

    v:vs = vars
    fi   = \case { F fv -> f fv; B i  -> vars !! i }
    f'   = \case { F fv -> f fv; B () -> v }

    vsN :: Int -> [Text]
    vsN n = drop n vars
