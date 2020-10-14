{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Moon.Atom
  ( Atom(..)
  ) where

import Prelude ()
import ClassyPrelude
import GHC.Natural
import Urbit.Runic


-- Types -----------------------------------------------------------------------

data Atom
  = INT Integer
  | NAT Natural
  | SYM Text
  | WOR Natural Natural
  | BUF Natural [Natural]
 deriving (Eq, Ord)


-- Instances -------------------------------------------------------------------

showInt :: Integer -> String
showInt i | i >= 0 = '+' : show i
showInt i          = show i

instance Show Atom where
  show = unpack . runicShowWide

instance ToRunic Atom where
  toRunic = \case
    INT i    -> Leaf $ pack $ showInt i
    NAT n    -> Leaf $ pack $ show n
    SYM t    -> Leaf $ pack $ show t
    WOR n x  -> Leaf $ pack $ show x <> "w" <> show n
    BUF n xs -> Mode wid tal
     where
      wid = IFix (pack ("#" <> show n <> "[")) "]" ruz
      tal = RunN ":#" ruz
      ruz = Leaf . tshow <$> xs
