module Main (main) where

import Proxy.API
import Proxy.Options

main :: IO ()
main = parseConfigOrOptions >>= run


