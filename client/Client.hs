module Main (main) where

import System.Environment (getArgs)

import Client.Args
import Client.Connect

main :: IO ()
main = getArgs >>= parseArgs >>= processClient
