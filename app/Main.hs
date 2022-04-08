{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.USDT

main :: IO ()
main = do
    $(tracepoint "hello")
    putStrLn "Hello, Haskell!"
