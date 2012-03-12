module Format where

import Types

format :: (Type, Nick) -> String
format (Plain, Nick nick) = "[%s] <" ++ nick ++ ">: %s\n"
format (Action, Nick nick) = "[%s] *"++ nick ++ " %s\n"
format (System, _) = "[%s] %s\n"

timeFormat = "%H:%M:%S"

commandAction = "/me "
