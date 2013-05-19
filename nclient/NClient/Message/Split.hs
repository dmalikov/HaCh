{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module NClient.Message.Split (simple, words) where

import qualified Data.Text as T
import Prelude hiding (words)
import qualified Prelude

simple :: Integral a => T.Text -> a -> [T.Text]
simple m (fromIntegral -> n) = go m
  where go xs
          | T.length xs < n = [xs]
          | otherwise = let (a,b) = T.splitAt n xs
                        in a : go b

words :: Integral a => T.Text -> a -> [T.Text]
words m (fromIntegral -> n) = map (T.intercalate " " . reverse) . go [] $ T.words m
  where go :: [T.Text] -> [T.Text] -> [[T.Text]]
        go [] [] = []
        go [] (x:xs)
          | T.length x < n = go [x] xs
          | otherwise = let (a,b) = T.splitAt n x
                        in [a] : go [] (b:xs)
        go a [] = [a]
        go a (x:xs)
          | length a + sum (map T.length a) + T.length x <= n = go (x:a) xs
          | otherwise = a : go [] (x:xs)
