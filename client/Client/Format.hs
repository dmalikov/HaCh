
module Client.Format where

import Hach.Types

format :: S2C -> String
format (S2C _ (SPlain   nick) _) = "[%s] <" ++ nick ++ ">: %s\n"
format (S2C _ (SAction  nick) _) = "[%s] *" ++ nick ++ " %s\n"
format (S2C _ (SSetNick nick) _) = "[%s] "  ++ nick ++ " %s\n"
format (S2C _  SSystem        _) = "[%s] ! %s\n"

timeFormat :: String
timeFormat = "%H:%M:%S"

commandAction :: String
commandAction = "/me "

commandSetNick :: String
commandSetNick = "/nick "
