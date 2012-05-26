{-# LANGUAGE UnicodeSyntax #-}
module Server.Message where

import Hach.Types

connectedClientM ∷ Nick → Timestamp → S2C
connectedClientM η = S2C (η ++ " is connected.") SSystem

existedNickM ∷ Nick → Timestamp → S2C
existedNickM η = S2C ("Nickname " ++ η ++ " is already in use.") SSystem

leftClientM ∷  Nick → Timestamp → S2C
leftClientM η = S2C (η ++ " has left conversation.") SSystem

settedNickM ∷ Nick → Nick → Timestamp → S2C
settedNickM nickFrom nickTo = S2C ("is know as " ++ nickTo ++ ".") (SSetNick nickFrom)

undefinedNickM ∷ Timestamp → S2C
undefinedNickM = S2C "To join a chat please set another nick with /nick command." SSystem
