module NClient.Args (parseArgs) where

import System.Console.GetOpt

data Options = ServerIP String
             | Name String

options :: [OptDescr Options]
options =
  [ Option "s" ["server"] (ReqArg ServerIP "Server IP") "Set server IP address."
  , Option "n" ["nick"] (ReqArg Name "User nickname") "Set user nickname."
  ]

parseArgs :: [String] -> IO (String, String)
parseArgs argv = case getOpt Permute options argv of
  (os, _, []) ->
    let ips = [ s | ServerIP s <- os ]
        nicks = [ n | Name n <- os ]
    in case (ips, nicks) of
      ([ip], [nick]) -> return (ip, nick)
      (_, _) -> error $ usageInfo usage options
  (_, _, es) -> error $ concat es ++ usageInfo usage options
  where usage = "Usage: hach-nclient [OPTIONS]"

