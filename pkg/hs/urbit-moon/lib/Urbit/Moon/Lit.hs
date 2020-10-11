{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Moon.Lit
  ( Literal(..)
  ) where

import Prelude ()
import ClassyPrelude
import GHC.Natural
import Urbit.Runic


-- Types -----------------------------------------------------------------------

data Literal
  = LitInt Integer
  | LitNat Natural
  | LitStr Text
  | LitWorNat Natural Natural
  | LitWorInt Natural Integer
 deriving (Eq, Ord)

showInt :: Integer -> String
showInt i | i >= 0 = '+' : show i
showInt i          = show i

instance Show Literal where
  show = \case
    LitInt i      -> showInt i
    LitNat n      -> show n
    LitStr t      -> show t
    LitWorNat n x -> show x <> "w" <> show n
    LitWorInt n x -> showInt x <> "w" <> show n

instance ToRunic Literal where
  toRunic x = Leaf (tshow x)
