{-# LANGUAGE UnicodeSyntax #-}

module Hach.Format where

import Hach.Types

format ∷ S2C → String
format (SMessage nick _) = "[%s] <" ++ nick ++ ">: %s\n"
format (SAction  nick _) = "[%s] *" ++ nick ++ " %s\n"
format (SSetNick nick _) = "[%s] "  ++ nick ++ " %s\n"
format (SSystem       _) = "[%s] ! %s\n"

timeFormat ∷ String
timeFormat = "%H:%M:%S"

commandAction ∷ String
commandAction = "/me "

commandSetNick ∷ String
commandSetNick = "/nick "
