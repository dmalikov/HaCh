{-# LANGUAGE UnicodeSyntax #-}
module Server.Message where

import Hach.Types

connectedClientM ∷ Nick → S2C
connectedClientM nick = SSystem $ nick ++ " is connected."

existedNickM ∷ Nick → S2C
existedNickM nick = SSystem $ "Nickname " ++ nick ++ " is already in use."

leftClientM ∷ Nick → S2C
leftClientM nick = SSystem $ nick ++ " has left conversation."

settedNickM ∷ Nick → Nick → S2C
settedNickM nickFrom nickTo = SSetNick nickFrom $ "is know as " ++ nickTo ++ "."

undefinedNickM ∷ S2C
undefinedNickM = SSystem "To join a chat please set another nick with /nick command."
