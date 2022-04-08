{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.USDT

tp1 :: Tracepoint ()
tp1 = $(mkTracepoint' "tp1")

main :: IO ()
main = do
    triggerTracepoint tp1
    $(tracepoint "tp2")
    triggerTracepoint tp1
    putStrLn "Hello, Haskell!"
