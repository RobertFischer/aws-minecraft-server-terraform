module Main where

import AWS.Lambda.Runtime (jsonMain)
import Lib (handler)

main :: IO ()
main = jsonMain handler
