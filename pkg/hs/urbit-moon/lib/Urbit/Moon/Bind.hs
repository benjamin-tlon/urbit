{-# OPTIONS_GHC -Wall #-}

module Urbit.Moon.Bind (bind, unbind) where

import Bound
import ClassyPrelude
import Urbit.Moon.AST
import Urbit.Moon.Exp

import Control.Lens (view, over, _1, _2, _3)
import Data.List    (elemIndex, (!!))


-- Utils -----------------------------------------------------------------------

mkLet :: Eq a => [(a,Exp a,Exp a)] -> Exp a -> Exp a
mkLet [] b = b
mkLet bs b =
  Let (bs <&> abstr . view _2)
      (bs <&> abstr . view _3)
      (abstr b)
 where
  abstr = abstract (`elemIndex` map (view _1) bs)

letBinds :: [Bind] -> AST -> Exp Text
letBinds bs ast = mkLet (goBinder <$> bs) (bind ast)
 where
  goBinder (Bind n t x) = (n, bind t, bind x)

mapTup :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
mapTup f g (x,y) = (f x, g y)


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
    CON t n xs         -> Con (go t) n (go <$> xs)
    PAT t x pats       -> Pat (go $ VAR t) (go x) (goPat <$> pats)
    TYP x              -> Typ x
    NAT                -> Nat
    INT                -> Int
    DAT vs             -> Dat (fmap go <$> vs)
    VEC x              -> Vec (go x)
    WOR n              -> Wor n
    BUF n              -> Buf n
    FUN n t v          -> Fun (go t) (abstract1 n (go v))
    OPR o              -> Opr (go <$> o)
    LIT l              -> Lit l

  goPat :: Pat -> (Int, Scope Int Exp Text)
  goPat (MkPat vs x) = mapTup (fromIntegral . length)
                              (abstract (`elemIndex` vs) . bind)
                              (vs, x)


-- Reconstruct AST -------------------------------------------------------------

unbind :: Exp Text -> AST
unbind = go allVars id
 where
  allVars :: [Text]
  allVars = ((singleton <$> ['a'..'z']) <> gogo 2)
   where
    gogo :: Integer -> [Text]
    gogo n = (map (pack . (: show n)) ['a'..'z']) <> gogo (succ n)

  go :: ∀a. [Text] -> (a -> Text) -> Exp a -> AST
  go vars f = \case
    Lam b      -> LAM v (recurIn b)
    Var x      -> VAR (f x)
    App x y    -> APP (recur x) (recur y)
    Seq x y    -> SEQ (recur x) (recur y)
    Nat        -> NAT
    Int        -> INT
    Typ n      -> TYP n
    Wor n      -> WOR n
    Buf n      -> BUF n
    Dat ds     -> DAT (fmap recur <$> ds)
    Vec x      -> VEC (recur x)
    Fun t b    -> FUN v (recur t) (recurIn b)
    Let t x b  -> LET (goBinds t x) $ go (vsN (length x)) fi $ fromScope b
    Con t n xs -> CON (recur t) n (recur <$> xs)
    Pat n t bs -> case n of Var a -> PAT (f a) (recur t) (goPat <$> bs)
                            _     -> recur $ Let [abstract (const Nothing) t]
                                                 [abstract (const Nothing) n]
                                           $ Scope
                                           $ Pat (Var (B 0)) (F . Var <$> t)
                                           $ flip fmap bs
                                           $ over _2 (fmap (F . Var))
    Opr o      -> OPR (recur <$> o)
    Lit l      -> LIT l
   where
    recur :: Exp a -> AST
    recur = go vars f

    recurIn :: Scope () Exp a -> AST
    recurIn = go vs f' . fromScope

    goPat :: (Int, Scope Int Exp a) -> Pat
    goPat (n, sc) =
      let ni = fromIntegral n
      in MkPat (take ni vars)
               (go (vsN ni) fi (fromScope sc))

    goBind :: Int -> Scope Int Exp a -> Scope Int Exp a -> Bind
    goBind i typ val = Bind (vars !! i)
                            (go (vsN i) fi $ fromScope typ)
                            (go (vsN i) fi $ fromScope val)

    goBinds :: [Scope Int Exp a] -> [Scope Int Exp a] -> [Bind]
    goBinds = zipWith3 goBind [0..]

    v:vs = vars

    fi :: Var Int a -> Text
    fi = \case { F fv -> f fv; B i  -> vars !! i }

    f' :: Var () a -> Text
    f' = \case { F fv -> f fv; B () -> v }

    vsN :: Int -> [Text]
    vsN n = drop n vars
