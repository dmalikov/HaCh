{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module NClient.Message.Split (simple, words) where

import Data.List (intercalate)
import Prelude hiding (words)
import qualified Prelude

simple ∷ Integral α ⇒ α → String → [String]
simple (fromIntegral → n) = go
  where go xs
          | length xs < n = [xs]
          | otherwise = let (a,b) = splitAt n xs
                        in a : go b

words ∷ Integral α ⇒ α → String → [String]
words (fromIntegral → n) = map (intercalate " " . reverse) . go [] . Prelude.words
  where go ∷ [String] → [String] → [[String]]
        go [] [] = []
        go [] (x:xs)
          | length x < n = go [x] xs
          | otherwise = let (a,b) = splitAt n x
                        in [a] : go [] (b:xs)
        go a [] = [a]
        go a (x:xs)
          | length a + sum (map length a) + length x <= n = go (x:a) xs
          | otherwise = a : go [] (x:xs)
