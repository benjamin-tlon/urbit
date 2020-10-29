module Main where

import ClassyPrelude
import Urbit.Soon.Parse (execSoon)

main :: IO ()
main = do
  fil <- getArgs >>= \case
           [f] -> pure (unpack f)
           _   -> error "usage: urbit-soon file.soon"

  execSoon fil
