{-# LANGUAGE UnicodeSyntax #-}
module NClient.Message.History
  ( History
  , empty, prepend
  , line
  , next, previous
  ) where

import Data.Sequence (Seq, (<|))
import Prelude hiding (lines)
import qualified Data.Sequence as Seq

data History = History { lines ∷ Seq String, current ∷ Int, capacity ∷ Int }

prepend ∷ String → History → History
prepend l h = h { lines = Seq.take (capacity h) $ l <| lines h, current = 0 }

empty ∷ Int → History
empty c = History { lines = Seq.empty, current = 0, capacity = c }

line ∷ History → String
line h
  | current h == 0 = " "
  | otherwise = lines h `Seq.index` (pred $ current h)

next ∷ History → History
next h@(History ls i c)
  | i == c || i == Seq.length ls = h
next h = h { current = succ $ current h }

previous ∷ History → History
previous h@(History _ i _)
  | i == 0 = h
previous h = h { current = pred $ current h }
