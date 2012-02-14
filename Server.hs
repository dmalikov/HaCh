module Main where

import Control.Monad (forever, unless, void)
import Network.Socket

loop :: Socket -> IO ()
loop s = do r <- recv s 1024
            unless (null r) (void $ send s r)
            loop s

main :: IO ()
main = withSocketsDo $ do
         serveraddr:_ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "7123")
         sock <- socket (addrFamily serveraddr) Stream defaultProtocol
         setSocketOption sock ReuseAddr 1
         bindSocket sock (addrAddress serveraddr)
         listen sock 1
         (csock, _) <- accept sock
         forever (loop csock)
