{-# LANGUAGE OverloadedStrings #-}

module Server.Message where

import Data.Monoid ((<>))
import Hach.Types

connectedClientM :: Nick -> Timestamp -> S2C
connectedClientM n = S2C (n <> " is connected.") SSystem

existedNickM :: Nick -> Timestamp -> S2C
existedNickM n = S2C ("Nickname " <> n <> " is already in use.") SSystem

leftClientM ::  Nick -> Timestamp -> S2C
leftClientM n = S2C (n <> " has left conversation.") SSystem

settedNickM :: Nick -> Nick -> Timestamp -> S2C
settedNickM nickFrom nickTo = S2C ("is know as " <> nickTo <> ".") (SSetNick nickFrom)

undefinedNickM :: Timestamp -> S2C
undefinedNickM = S2C "To join a chat please set another nick with /nick command." SSystem
