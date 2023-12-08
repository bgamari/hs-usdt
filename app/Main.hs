{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Control.Concurrent
import System.Environment
import System.USDT
import Data.Proxy

-- how many primes to calculate in each thread
n_primes :: Int
n_primes = 5000

primes1 n done
  = do --putStrLn (show ((sieve [n..])!!n_primes))
       $(tracepoint' @Int Proxy "start") n
       show ((sieve [n..])!!n_primes) `seq` return ()
       putMVar done ()
       $(tracepoint' @Int Proxy "end") n

sieve (p:xs) = p : sieve [x | x <- xs, not (x `mod` p == 0)]

main
  = runInUnboundThread $ do
       --[str] <- getArgs
       --let instances = read str :: Int
       let instances = 20
       dones <- replicateM instances newEmptyMVar
       sequence_ [forkIO (primes1 (i+2) (dones!!i)) | i <- [0..instances-1]]
       sequence_ [takeMVar (dones!!i) | i <- [0..instances-1]]
