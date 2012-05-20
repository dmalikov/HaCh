{-# LANGUAGE UnicodeSyntax #-}
module Hach.Types where

type Nick = String
type Text = String

data S2C = SMessage Nick Text
         | SAction Nick Text
         | SSetNick Nick Text
         | SSystem Text
           deriving (Read, Show)

data C2S = CMessage Text
         | CAction Text
         | CSetNick Text
           deriving (Read, Show)

class Message α where
  text ∷ α → String

instance Message S2C where
  text (SMessage _ τ) = τ
  text (SAction _ τ) = τ
  text (SSetNick _ τ) = τ
  text (SSystem τ) = τ

instance Message C2S where
  text (CMessage τ) = τ
  text (CAction τ) = τ
  text (CSetNick τ) = τ
