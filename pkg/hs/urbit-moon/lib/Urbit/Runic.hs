{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Runic
  ( Runic(..)
  , ToRunic(..)
  , runicShow
  , runicShowWide
  , appRune
  ) where

import ClassyPrelude

import qualified Data.Text as T


-- Runic Tree ------------------------------------------------------------------

data Runic
    = Leaf Text
    | RunC Text [Runic]
    | RunN Text [Runic]
    | Jog0 Text [(Runic, Runic)]
    | Jog1 Text Runic [(Runic, Runic)]
    | Cor0 Text [(Runic, Runic)]
    | Cor1 Text Runic [(Runic, Runic)]
    | IFix Text Text [Runic]
    | JFix Text Text [(Runic, Runic)]
    | Bind Text Runic
    | Pair Text Runic Runic
    | Wide Runic
    | Pref Text Runic
    | Tied Runic Runic
    | Mode Runic Runic
  deriving (Show)


-- To Runic --------------------------------------------------------------------

instance IsString Runic where
 fromString = Leaf . pack

class ToRunic a where
  toRunic ∷ a → Runic

runicShow :: ToRunic a => a -> Text
runicShow = tall . toRunic

runicShowWide :: ToRunic a => a -> Text
runicShowWide = wide . toRunic

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
        Cor0 i xs   → go (Jog0 i xs)
        Cor1 i x xs → go (Jog1 i x xs)
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
                      g:gs -> indent d t <> "  " <> wide g <> "\n"
                           <> bod (length gs - 1) gs
          where bod _ []     = ""
                bod n (g:gs) = go (d + n*2) g <> bod (pred n) gs

        RunN t xs → fromMaybe (runNDent d t xs) (runNInline d t xs)

        Jog0 t xs → mconcat ([line d t] <> bod <> [line d "=="])
          where bod = fromMaybe (jogTallBody d xs) (jogWideBody d xs)

        Jog1 t x xs → mconcat ([line d (t<>hed)] <> bod <> [line d "=="])
          where bod = fromMaybe (jogTallBody d xs) (jogWideBody d xs)
                hed = "  " <> wide x

        Cor0 t xs → mconcat ([line d t] <> bod <> [line d "=="])
          where bod = fromMaybe (corTallBody d xs) (corWideBody d xs)

        Cor1 t x xs → mconcat ([line d (t<>hed)] <> bod <> [line d "=="])
          where bod = fromMaybe (corTallBody d xs) (corWideBody d xs)
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
    runNInline _ _ [] = Nothing
    runNInline d t xs = do
        let bod = T.lines $ mconcat $ fmap (go (d+4)) xs
            wid = maximumEx $ fmap length bod
        bodNE <- fromNullable bod
        guard (wid < 80)
        let (b, bs)   = splitFirst bodNE
        let muck hed = indent d t <> "  " <> T.strip hed
        pure $ unlines $ [muck b] <> bs <> [indent d "=="]

    jogTallBody d = fmap (\(h,t) → go (d+2) h <> go (d+4) t)

    corTallBody d = fmap (\(h,t) → luslus (go d h) <> go (d+2) t)

    luslus :: Text -> Text
    luslus = unlines . fmap f . lines
     where
      f (T.span (== ' ') -> (ind, tex)) = ind <> "++  " <> tex

    corWideBody ∷ Int → [(Runic, Runic)] → Maybe [Text]
    corWideBody _ [] = Nothing
    corWideBody d xs = do
        let heads  = fst <$> xs
            hedWid = maximumEx (length . wide <$> heads) :: Int
        sequence $ xs <&> \(h,t) → do
            let hed = "++  " <> wide h
            let gap = T.replicate (6 + (hedWid - length hed)) " "
            let lin = hed <> gap <> wide t
            guard (length lin <= (53 - d))
            pure (line d lin)

    jogWideBody ∷ Int → [(Runic, Runic)] → Maybe [Text]
    jogWideBody _ [] = Nothing
    jogWideBody d xs = do
        let heads  = fst <$> xs
            hedWid = maximumEx (length . wide <$> heads) :: Int
        sequence $ xs <&> \(h,t) → do
            let hed = wide h
            let gap = T.replicate (2 + (hedWid - length hed)) " "
            let lin = wide h <> gap <> wide t
            guard (length lin <= (53 - d))
            pure (line (d+2) lin)

appRune :: [Runic] -> Runic
appRune xs = Mode wid tal
 where
  wid = IFix "(" ")" xs
  tal = case length xs of
          2 -> RunC "%-" xs
          3 -> RunC "%+" xs
          4 -> RunC "%^" xs
          _ -> RunN "%*" xs
