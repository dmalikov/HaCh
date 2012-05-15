{-# LANGUAGE UnicodeSyntax #-}

module Format where

import Types

format ∷ (Type, Nick) → String
format (Plain, Nick nick) = "[%s] <" ++ nick ++ ">: %s\n"
format (Action, Nick nick) = "[%s] *"++ nick ++ " %s\n"
format (SetNick, _) = ""
format (System, Nick nick) = "[%s] " ++ nick ++ " %s\n"

timeFormat = "%H:%M:%S"

commandAction = "/me "
commandSetNick = "/nick "
