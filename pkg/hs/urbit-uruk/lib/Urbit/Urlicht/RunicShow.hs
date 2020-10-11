module Urbit.Urlicht.RunicShow where

import ClassyPrelude

import Data.Function ((&))
import qualified Data.Text as T
import Data.Void

import qualified Urbit.Urlicht.CST as C
import qualified Urbit.Urlicht.Hoon as H
import qualified Urbit.Urlicht.HoonToSimple as H2S
import qualified Urbit.Urlicht.Simple as S
import qualified Urbit.Urlicht.SimpleToCoreHack as S2C
import qualified Urbit.Urlicht.Core as Core
import qualified Urbit.Urlicht.Errors as E
import qualified Urbit.Urlicht.Meta as Meta
import qualified Urbit.Noun as N

class RunicShow a where
  runic :: a -> Text

instance RunicShow C.CST where
  runic = tall . toRunic

instance RunicShow (H.Hoon Text) where
  runic = runic . C.concretize

instance RunicShow (S.Simple Text) where
  runic = runic . H2S.up

instance RunicShow (Core.Core Text) where
  runic = runic . S2C.up

instance RunicShow (Core.Value Text) where
  runic = runic . Core.quote

instance RunicShow (Core.Value Void) where
  runic v = runic (fmap absurd v :: Core.Value Text)

-- TODO how bad is UndecidableInstances?
-- instance (Functor f, Unvar a, RunicShow (f Text)) => RunicShow (f a) where
--   runic = runic . fmap unvar

instance RunicShow E.Error where
  runic = \case
    E.EUnknown -> "Very sorry sir, but your program does not compile."
    E.EUnify -> "Ah, excuse me, but it seems some terms don't unify."
    E.ESpine -> "If you would please refrain from applying metas to non-vars."
    E.EScope -> "Please sir! Your extraneous vars make unification quite hard."
    E.EOccurs -> "Begging your pardon sir, but I never did understand recursion."
    E.EName -> "I'm afraid I don't recognize that variable, sir."
    E.ENotFun -> "A function type, if you please."
  {-runic = \case
    D.NestFail t u ->
      "[nest-fail]\nACTUAL:\n" <>
      runic (D.unvar <$> t) <>
      "\nEXPECTED:\n" <>
      runic (D.unvar <$> u)
    D.NotTyp t -> "[not-hax]\n" <> runic (D.unvar <$> t)
    D.NotFun t -> "[not-hax]\n" <> runic (D.unvar <$> t)
    D.NotCel t -> "[not-hax]\n" <> runic (D.unvar <$> t)
    D.NotAtm t -> "[not-atm]\n" <> runic (D.unvar <$> t)
    D.NotWut t -> "[not-hax]\n" <> runic (D.unvar <$> t)
    D.NotHaxBuc t -> "[not-$%]\n" <> runic (D.unvar <$> t)
    D.Other s ->
      "[other] Your program has the type error '" <>
      s <> "'. This probably corresponds to a `guard`" <>
      " condition in Deppy.Core. Terribly sorry."-}

data Runic
    = Leaf Text
    | RunC Text [Runic]
    | RunN Text [Runic]
    | Jog0 Text [(Runic, Runic)]
    | Jog1 Text Runic [(Runic, Runic)]
    | IFix Text Text [Runic]
    | JFix Text Text [(Runic, Runic)]
    | Bind Text Runic
    | Pair Text Runic Runic
    | Wide Runic
    | Pref Text Runic
    | Tied Runic Runic
    | Mode Runic Runic
  deriving (Show)

wide ∷ Runic → Text
wide = go
  where
    go = \case
        Leaf t      → t
        RunC t xs   → mconcat [t, "(", intercalate " " (go <$> xs), ")"]
        RunN t xs   → mconcat [t, "(", intercalate " " (go <$> xs), ")"]
        IFix h t xs → mconcat [h, intercalate " " (go <$> xs), t]
        JFix h t xs → mconcat [h, intercalate ", " (pair go <$> xs), t]
        Bind t v    → mconcat [t, "/", go v]
        Pair i h t  → mconcat [go h, i, go t]
        Jog0 i xs   → i <> "(" <> bod <> ")"
          where bod = intercalate ", " (xs <&> (\(h,t) → go h <> " " <> go t))
        Jog1 i x [] → i <> "(" <> go x <> ")"
        Jog1 i x xs → i <> "(" <> go x <> "; " <> bod <> ")"
          where bod = intercalate ", " $ xs <&> (\(h,t) → go h <> " " <> go t)
        Wide x      → go x
        Pref t x    → t <> go x
        Tied x y    → go x <> go y
        Mode w _    → go w

    pair f (x, y) = f x <> " " <> f y

tall ∷ Runic → Text
tall = go 0
  where
    go d (wide -> t) | length t < 40 = line d t
    go d v                           = ta d v

    indent d t = replicate d ' ' <> t

    line d t = indent d t <> "\n"

    ta d = \case
        Leaf t → line d t

        RunC t xs → case xs of
                      []   -> line d t <> bod (length xs - 1) xs
                      x:xs -> indent d t <> "  " <> wide x <> "\n"
                           <> bod (length xs - 1) xs
          where bod n []     = ""
                bod n (x:xs) = go (d + n*2) x <> bod (pred n) xs

        RunN t xs → fromMaybe (runNDent d t xs) (runNInline d t xs)

        Jog0 t xs → mconcat ([line d t] <> bod <> [line d "=="])
          where bod = fromMaybe (jogTallBody d xs) (jogWideBody d xs)

        Jog1 t x xs → mconcat ([line d (t<>hed)] <> bod <> [line d "=="])
          where bod = fromMaybe (jogTallBody d xs) (jogWideBody d xs)
                hed = "  " <> wide x

        Mode _ t → go d t

        IFix h t xs → line d $ wide $ IFix h t xs
        JFix h t xs → line d $ wide $ JFix h t xs
        Bind t v    → line d $ wide $ Bind t v
        Pair i h t  → line d $ wide $ Pair i h t
        Wide x      → line d $ wide x
        Pref t x    → line d $ wide $ Pref t x
        Tied x y    → line d $ wide $ Tied x y

    runNDent ∷ Int → Text → [Runic] → Text
    runNDent d t xs = mconcat $ [line d t] <> (go (d+2) <$> xs) <> [line d "=="]

    runNInline :: Int -> Text -> [Runic] -> Maybe Text
    runNInline d t [] = Nothing
    runNInline d t xs = do
        let bod = T.lines $ mconcat $ fmap (go (d+4)) xs
            wid = maximumEx $ fmap length bod
        bod <- fromNullable bod
        guard (wid < 80)
        let (b, bs)   = splitFirst bod
        let muck head = indent d t <> "  " <> T.strip head
        pure $ unlines $ [muck b] <> bs <> [indent d "=="]

    jogTallBody d = fmap (\(h,t) → go (d+2) h <> go (d+4) t)

    jogWideBody ∷ Int → [(Runic, Runic)] → Maybe [Text]
    jogWideBody d [] = Nothing
    jogWideBody d xs = do
        let heads  = fst <$> xs
            hedWid = maximumEx (length . wide <$> heads) :: Int
        sequence $ xs <&> \(h,t) → do
            let hed = wide h
            let gap = T.replicate (2 + (hedWid - length hed)) " "
            let lin = wide h <> gap <> wide t
            guard (length lin <= (53 - d))
            pure (line (d+2) lin)

toRunic ∷ C.CST → Runic
toRunic = go
  where
    go = \case
        C.Hax          -> Leaf "#"
        C.Pat          -> Leaf "@"
        C.Hol          -> Leaf "_"
        C.Var t        -> Leaf t
        C.Met m        -> Leaf (Meta.showMeta m)
        C.Nat a        -> tagLit a
        C.Col a x      -> appTag a x
        C.Hed x        -> hed x
        C.DotGal x     -> hed x
        C.Tal x        -> tal x
        C.DotGar x     -> tal x
        C.HaxBuc xs    -> tagUnion xs
        C.Obj cs       -> recLit cs
        C.BarCen cs    -> recLit cs
        C.HaxCen xs    -> recTy xs
        C.Cls xs       -> recTy xs
        C.Lam bs x     -> lambda bs x
        C.BarTis bs x  -> lambda bs x
        C.Fun bs x     -> pie bs x
        C.HaxHep bs x  -> pie bs x
        C.Cel bs x     -> cellTy bs x
        C.HaxCol bs x  -> cellTy bs x
        C.Wut w        -> wut w
        C.Cns xs       -> cellLit xs
        C.ColHep x y   -> cellLit [x, y]
        C.ColTar xs    -> cellLit xs
        C.App xs       -> apply (go <$> xs)
        C.CenDot x y   -> apply [go y, go x]
        C.CenHep x y   -> apply [go x, go y]
        C.The x y      -> the x y
        C.KetFas x y   -> the y x
        C.KetHep x y   -> the x y
        C.Fas x y      -> the y x
        C.TisFas t x y -> let_ t x y
        C.DotDot x y   -> fix x y
        C.WutCen x cs  -> switch x cs
        C.WutCol x y z -> RunC "?:" [go x, go y, go z]
        C.Lus x        -> lus x
        C.DotLus x     -> lus x
        C.Tis x y      -> tis x y
        C.DotTis x y   -> tis x y

    lus x = Leaf "LUS"
    tis x y = Leaf "TIS"

    tagLit a = tag "%" "" a

    appTag a x = Mode wide tall
      where wide = Pair ":" (tag "" "" a) (go x)
            tall = apply [go x, tagLit a]

    hed x = Mode wide tall
      where wide = Pref "-." (go x)
            tall = RunC ".<" [go x]

    tal x = Mode wide tall
      where wide = Pref "+." (go x)
            tall = RunC ".>" [go x]

    tagUnion xs = Jog0 "$%" $ jog (tag "" "") go xs

    recTy xs = Mode wide tall
      where wide = JFix "{|" "|}" $ entJog $ mapToList xs
            tall = Jog0 "$`" $ jog (tag "" "") go xs

    pie bs x = Mode wide tall
      where wide = IFix "<|" "|>" $ fmap binder bs <> [go x]
            tall = RunN "$-" $ fmap binder bs <> [go x]

    switch x cs = Jog1 "?%" (go x) (jog (tag "%" "") go cs)

    {-switch' x cs = Jog1 "?#" (go x) (mkCase <$> mapToList cs)
      where
        mkCase (atm, (v, c)) = (IFix "[" "]" [tag "%" "" atm, Leaf v], go c)-}

    recLit cs = Mode wide tall
      where wide = JFix "{" "}" $ entJog $ mapToList cs
            tall = Jog0 "|%" (entJog $ mapToList cs)

    fix x y = RunC ".." [binder x, go y]

    the x y = Mode wide tall
      where wide = Tied (IFix "`" "`" [go x]) (go y)
            tall = RunC "^-" [go x, go y]

    let_ b x y = RunC "=/" [binder b, go x, go y]

    apply xs = Mode wide tall
      where wide = IFix "(" ")" xs
            tall = case length xs of
                     2 -> RunC "%-" xs
                     n -> RunN "%*" xs

    lambda bs x = Mode wide tall
      where wide = IFix "<" ">" (fmap binder bs <> [go x])
            tall = case bs of
                     []  -> go x
                     [b] -> RunC "|=" [binder b, go x]
                     bs  -> RunC "|=" [IFix "(" ")" (binder<$>bs), go x]

    cellTy bs x = Mode wide tall
      where
        wide = IFix "[|" "|]" (fmap binder bs <> [go x])
        tall = RunN "$:" (fmap binder bs <> [go x])

    cellLit xs = Mode wide tall
      where wide = IFix "[" "]" (go <$> xs)
            tall = xs & \case [x,y] → RunC ":-" [go x, go y]
                              _     → RunN ":*" (go <$> xs)

    jog ∷ Ord k => (k → Runic) → (v → Runic) → Map k v → [(Runic, Runic)]
    jog x y = fmap (\(k,v) -> (x k, y v)) . mapToList

    wut w = setToList w & \case
        [x] -> tag "$" "$" x
        xs  -> Wide $ RunN "?" (tag "" "" <$> xs)

    entJog ∷ [(N.Atom, C.CST)] → [(Runic, Runic)]
    entJog xs = xs <&> \(h,t) → (tag "" "" h, go t)

    binder ( Nothing, x ) = go x
    binder ( Just t,  x ) = Bind t (go x)

    tag t n 0 = Leaf (n <> "0")
    tag t n x = N.fromNoun (N.A x) & \case
        Just (N.Cord c) | okay c -> Leaf (t <> c)
        _                        -> Leaf (n <> tshow x)
      where
        okay xs = not (null xs) && all (`elem` ('-':['a'..'z'])) xs
