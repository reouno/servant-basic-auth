module Main where

import Authentication (basicAuthMain)

main :: IO ()
main = putStrLn "Listening at 8080..." >> basicAuthMain
