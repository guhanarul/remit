{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Handler.Warp (run)
import Backend.Server (app)

main :: IO ()
main = do
    putStrLn "We are accepting your letters at port 8080"
    run 8080 app
