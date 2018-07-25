module Main where

import Network.Wai.Handler.Warp

import Data
import Api

main :: IO ()
main = run 8081 salaryApp

