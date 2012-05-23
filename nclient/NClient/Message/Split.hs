{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module NClient.Message.Split (simple, words) where

import Prelude hiding (words)

simple ∷ Integral α ⇒ α → String → [String]
simple (fromIntegral → n) = go
  where go xs
          | length xs < n = [xs]
          | otherwise = let (a,b) = splitAt n xs
                        in a : go b

words ∷ Int → String → [String]
words = undefined
