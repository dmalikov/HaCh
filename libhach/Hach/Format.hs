{-# LANGUAGE UnicodeSyntax #-}

module Hach.Format where

import Hach.Types

format ∷ S2C → String
format (SMessage (Nick nick) _) = "[%s] <" ++ nick ++ ">: %s\n"
format (SAction  (Nick nick) _) = "[%s] *"++ nick ++ " %s\n"
format (SSetNick (Nick nick) _) = "[%s] " ++ nick ++ " %s\n"
format (SSystem              _) = "[%s] ! %s\n"

timeFormat ∷ String
timeFormat = "%H:%M:%S"

commandAction ∷ String
commandAction = "/me "

commandSetNick ∷ String
commandSetNick = "/nick "
