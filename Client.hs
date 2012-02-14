module Main where

import Control.Monad (forever)
import Network.Socket

loop :: Socket -> IO ()
loop s = getLine >>= send s >> recv s 1024 >>= putStrLn

main :: IO ()
main = withSocketsDo $
         do serveraddr:_ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "7123")
            sock <- socket (addrFamily serveraddr) Stream defaultProtocol
            connect sock (addrAddress serveraddr)
            forever (loop sock)
