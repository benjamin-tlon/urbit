module Urbit.Moon.Bind where

import Prelude ()
import Bound
import ClassyPrelude hiding ((\\))
import GHC.Natural

import Data.Deriving        (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Functor.Classes (showsPrec1)
import Data.List            (elemIndex, (!!), (\\))

import Urbit.Moon.AST hiding (Nat)
import Urbit.Moon.Exp

--------------------------------------------------------------------------------

let_ :: Eq a => [(a,Exp a)] -> Exp a -> Exp a
let_ [] b = b
let_ bs b = Let (map (abstr . snd) bs) (abstr b)
  where abstr = abstract (`elemIndex` map fst bs)

letBinds :: [Bind] -> AST -> Exp Text
letBinds bs x = let_ (bs <&> \(Bind n x) -> (n, bind x)) (bind x)

bindPat :: Pat -> (Nat, Scope Int Exp Text)
bindPat (MkPat vs x) =
  (,) (fromIntegral $ length vs)
      (abstract (`elemIndex` vs) $ bind x)

bind :: AST -> Exp Text
bind = go
 where
  go = \case
    VAR t              -> Var t
    APP x y            -> App (go x) (go y)
    SEQ x y            -> Seq (go x) (go y)
    LAM (Bind v b)     -> Lam (abstract1 v (go b))
    LET bs x           -> letBinds bs x
    CON (MkCon l xs r) -> Con l (go <$> xs) r
    PAT x pats         -> Pat (go x) (bindPat <$> pats)

    NAT_LIT n     -> NatLIT n
    NAT_INT x     -> NatINT (go x)
    NAT_RUN x z i -> NatRUN (go x) (go z) (go i)
    NAT_ZER x     -> NatZER (go x)
    NAT_INC x     -> NatINC (go x)
    NAT_DEC x     -> NatDEC (go x)
    NAT_EQL x y   -> NatEQL (go x) (go y)
    NAT_MUL x y   -> NatMUL (go x) (go y)
    NAT_DIV x y   -> NatDIV (go x) (go y)
    NAT_ADD x y   -> NatADD (go x) (go y)
    NAT_SUB x y   -> NatSUB (go x) (go y)
    NAT_MOD x y   -> NatMOD (go x) (go y)

    INT_LIT n   -> IntLIT n
    INT_NAT x   -> IntNAT (go x)
    INT_ZER x   -> IntZER (go x)
    INT_INC x   -> IntINC (go x)
    INT_DEC x   -> IntDEC (go x)
    INT_NEG x   -> IntNEG (go x)
    INT_EQL x y -> IntEQL (go x) (go y)
    INT_MUL x y -> IntMUL (go x) (go y)
    INT_DIV x y -> IntDIV (go x) (go y)
    INT_ADD x y -> IntADD (go x) (go y)
    INT_SUB x y -> IntSUB (go x) (go y)
    INT_MOD x y -> IntMOD (go x) (go y)

    VEC_LIT xs    -> VecLIT (go <$> xs)
    VEC_GEN x b   -> VecGEN (go x) (go b)
    VEC_IDX x y   -> VecIDX (go x) (go y)
    VEC_SET x y z -> VecSET (go x) (go y) (go z)
    VEC_UPD x y b -> VecUPD (go x) (go y) (go b)
    VEC_LIS x     -> VecLIS (go x)
    VEC_MAP x b   -> VecMAP (go x) (go b)
    VEC_CAT x y   -> VecCAT (go x) (go y)

    WOR_BIT_NOT  n x   -> WorBitNOT  n (go x)
    WOR_BIT_OR   n x y -> WorBitOR   n (go x) (go y)
    WOR_BIT_AND  n x y -> WorBitAND  n (go x) (go y)
    WOR_BIT_NAND n x y -> WorBitNAND n (go x) (go y)
    WOR_BIT_NOR  n x y -> WorBitNOR  n (go x) (go y)
    WOR_BIT_XOR  n x y -> WorBitXOR  n (go x) (go y)
    WOR_BIT_XNOR n x y -> WorBitXNOR n (go x) (go y)

    WOR_NAT_LIT n v   -> WorNatLIT n v
    WOR_NAT_MAK n x y -> WorNatMAK n (go x) (go y)
    WOR_NAT_NAT n x   -> WorNatNAT n (go x)
    WOR_NAT_ZER n x   -> WorNatZER n (go x)
    WOR_NAT_INC n x   -> WorNatINC n (go x)
    WOR_NAT_DEC n x   -> WorNatDEC n (go x)
    WOR_NAT_SUB n x y -> WorNatSUB n (go x) (go y)
    WOR_NAT_MUL n x y -> WorNatMUL n (go x) (go y)

    WOR_INT_LIT n v   -> WorIntLIT n v
    WOR_INT_MAK n x y -> WorIntMAK n (go x) (go y)
    WOR_INT_INT n x   -> WorIntINT n (go x)
    WOR_INT_ZER n x   -> WorIntZER n (go x)
    WOR_INT_NEG n x   -> WorIntNEG n (go x)
    WOR_INT_INC n x   -> WorIntINC n (go x)
    WOR_INT_DEC n x   -> WorIntDEC n (go x)
    WOR_INT_SUB n x y -> WorIntSUB n (go x) (go y)
    WOR_INT_MUL n x y -> WorIntMUL n (go x) (go y)

    BUF_LIT n vs    -> BufLIT n vs
    BUF_STR   tx    -> BufSTR   tx
    BUF_GEN n x b   -> BufGEN n (go x) (go b)
    BUF_IDX n x i   -> BufIDX n (go x) (go i)
    BUF_SET n x i v -> BufSET n (go x) (go i) (go v)
    BUF_UPD n x i b -> BufUPD n (go x) (go i) (go b)
    BUF_LIS n x     -> BufLIS n (go x)
    BUF_MAP n x b   -> BufMAP n (go x) (go b)
    BUF_ITR n x b   -> BufITR n (go x) (go b)
    BUF_CAT n x y   -> BufCAT n (go x) (go y)

allVars :: [Text]
allVars = pack <$> ((singleton <$> ['a'..'z']) <> go 2)
 where
  go n = (map (: show n) ['a'..'z']) <> go (succ n)

unbind :: Exp Text -> AST
unbind = go allVars id
 where
  go :: [Text] -> (a -> Text) -> Exp a -> AST
  go vars f = \case
    Lam b      -> LAM (Bind v $ go vs f' $ fromScope b)
    Var x      -> VAR (f x)
    App x y    -> APP (go vars f x) (go vars f y)
    Seq x y    -> SEQ (go vars f x) (go vars f y)
    Let x b    -> LET (unbindBinds x) $ go (vsN (length x)) fi $ fromScope b
    Con l xs r -> CON (MkCon l (go vars f <$> xs) r)
    Pat x bs   -> PAT (go vars f x) (unbindPat <$> bs)

    NatLIT n     -> NAT_LIT n
    NatINT x     -> NAT_INT (go vars f x)
    NatRUN x z i -> NAT_RUN (go vars f x) (go vars f z) (go vars f i)
    NatZER x     -> NAT_ZER (go vars f x)
    NatINC x     -> NAT_INC (go vars f x)
    NatDEC x     -> NAT_DEC (go vars f x)
    NatEQL x y   -> NAT_EQL (go vars f x) (go vars f y)
    NatMUL x y   -> NAT_MUL (go vars f x) (go vars f y)
    NatDIV x y   -> NAT_DIV (go vars f x) (go vars f y)
    NatADD x y   -> NAT_ADD (go vars f x) (go vars f y)
    NatSUB x y   -> NAT_SUB (go vars f x) (go vars f y)
    NatMOD x y   -> NAT_MOD (go vars f x) (go vars f y)

    IntLIT n   -> INT_LIT n
    IntNAT x   -> INT_NAT (go vars f x)
    IntZER x   -> INT_ZER (go vars f x)
    IntINC x   -> INT_INC (go vars f x)
    IntDEC x   -> INT_DEC (go vars f x)
    IntNEG x   -> INT_NEG (go vars f x)
    IntEQL x y -> INT_EQL (go vars f x) (go vars f y)
    IntMUL x y -> INT_MUL (go vars f x) (go vars f y)
    IntDIV x y -> INT_DIV (go vars f x) (go vars f y)
    IntADD x y -> INT_ADD (go vars f x) (go vars f y)
    IntSUB x y -> INT_SUB (go vars f x) (go vars f y)
    IntMOD x y -> INT_MOD (go vars f x) (go vars f y)

    VecLIT xs    -> VEC_LIT (go vars f <$> xs)
    VecGEN x b   -> VEC_GEN (go vars f x) (go vars f b)
    VecIDX x y   -> VEC_IDX (go vars f x) (go vars f y)
    VecSET x y z -> VEC_SET (go vars f x) (go vars f y) (go vars f z)
    VecUPD x y b -> VEC_UPD (go vars f x) (go vars f y) (go vars f b)
    VecLIS x     -> VEC_LIS (go vars f x)
    VecMAP x b   -> VEC_MAP (go vars f x) (go vars f b)
    VecCAT x y   -> VEC_CAT (go vars f x) (go vars f y)

    WorBitNOT  n x   -> WOR_BIT_NOT  n (go vars f x)
    WorBitOR   n x y -> WOR_BIT_OR   n (go vars f x) (go vars f y)
    WorBitAND  n x y -> WOR_BIT_AND  n (go vars f x) (go vars f y)
    WorBitNAND n x y -> WOR_BIT_NAND n (go vars f x) (go vars f y)
    WorBitNOR  n x y -> WOR_BIT_NOR  n (go vars f x) (go vars f y)
    WorBitXOR  n x y -> WOR_BIT_XOR  n (go vars f x) (go vars f y)
    WorBitXNOR n x y -> WOR_BIT_XNOR n (go vars f x) (go vars f y)

    WorNatLIT n v   -> WOR_NAT_LIT n v
    WorNatMAK n x y -> WOR_NAT_MAK n (go vars f x) (go vars f y)
    WorNatNAT n x   -> WOR_NAT_NAT n (go vars f x)
    WorNatZER n x   -> WOR_NAT_ZER n (go vars f x)
    WorNatINC n x   -> WOR_NAT_INC n (go vars f x)
    WorNatDEC n x   -> WOR_NAT_DEC n (go vars f x)
    WorNatSUB n x y -> WOR_NAT_SUB n (go vars f x) (go vars f y)
    WorNatMUL n x y -> WOR_NAT_MUL n (go vars f x) (go vars f y)

    WorIntLIT n v   -> WOR_INT_LIT n v
    WorIntMAK n x y -> WOR_INT_MAK n (go vars f x) (go vars f y)
    WorIntINT n x   -> WOR_INT_INT n (go vars f x)
    WorIntZER n x   -> WOR_INT_ZER n (go vars f x)
    WorIntNEG n x   -> WOR_INT_NEG n (go vars f x)
    WorIntINC n x   -> WOR_INT_INC n (go vars f x)
    WorIntDEC n x   -> WOR_INT_DEC n (go vars f x)
    WorIntSUB n x y -> WOR_INT_SUB n (go vars f x) (go vars f y)
    WorIntMUL n x y -> WOR_INT_MUL n (go vars f x) (go vars f y)

    BufLIT n ws    -> BUF_LIT n ws
    BufSTR   tx    -> BUF_STR   tx
    BufGEN n x b   -> BUF_GEN n (go vars f x) (go vars f b)
    BufIDX n x i   -> BUF_IDX n (go vars f x) (go vars f i)
    BufSET n x i v -> BUF_SET n (go vars f x) (go vars f i) (go vars f v)
    BufUPD n x i b -> BUF_UPD n (go vars f x) (go vars f i) (go vars f b)
    BufLIS n x     -> BUF_LIS n (go vars f x)
    BufMAP n x b   -> BUF_MAP n (go vars f x) (go vars f b)
    BufITR n x b   -> BUF_ITR n (go vars f x) (go vars f b)
    BufCAT n x y   -> BUF_CAT n (go vars f x) (go vars f y)

   where
    unbindPat (n, sc) =
      MkPat (take ni vars)
            (go (vsN ni) fi (fromScope sc))
     where
      ni = fromIntegral n

    unbindBind i = Bind (vars !! i) . go vars fi . fromScope

    unbindBinds = zipWith unbindBind [0..]

    fi = \case { F fv -> f fv; B i  -> vars !! i }
    f' = \case { F fv -> f fv; B () -> v }

    v :: Text
    vs :: [Text]
    (v, vs) = case vars of v:vs -> (v,vs)
                           _    -> error "impossible: end of infinite stream"

    vsN :: Int -> [Text]
    vsN n = drop n vars
