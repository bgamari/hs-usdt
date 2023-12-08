{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Proxy
import System.USDT
import Data.Tuple

tp1 :: Tracepoint Int
tp1 = $(mkTracepoint' (Proxy @Int) "tp1")

main :: IO ()
main = do
    triggerTracepoint tp1 (42)
    $(tracepoint "tp2" ())
    triggerTracepoint tp1 (52)
    putStrLn "Hello, Haskell!"
