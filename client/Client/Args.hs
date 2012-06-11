{-# LANGUAGE UnicodeSyntax #-}

module Client.Args (parseArgs) where

import System.Console.GetOpt

data Flag = ServerIP String
          | ClientNick String

options ∷ [OptDescr Flag]
options =
  [ Option "s" ["server"] (ReqArg ServerIP "server_ip") "set server ip adress"
  , Option "n" ["nick"] (ReqArg ClientNick "user nickname") "set user nickname"
  ]

parseArgs ∷ [String] → IO (String, String)
parseArgs argv = case getOpt Permute options argv of
  (os, _, []) → do
    let ips = [ s | ServerIP s ← os ]
    let nicks = [ n | ClientNick n ← os ]
    case (ips, nicks) of
      ([ip], [nick]) → return (ip, nick)
      (_, _) → error $ usageInfo usage options
  (_, _, es) → error $ concat es ++ usageInfo usage options
  where usage = "Usage: hach-client [OPTIONS...]"
