module Main (main) where

import System.Environment (getArgs)

import NClient.Args
import NClient.Connect
import NClient.GUI

main :: IO ()
main = getArgs >>= parseArgs >>= connect >>= gui
