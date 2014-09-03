{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : MPL-2.0
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  src/systemK/ThreadManager.hs
-}

module Main where

import SystemK.Utils.Panic (panic)

main :: IO ()
main  = do
  putStrLn "Goodbye systemd world"
  panic "systemd" ["oh", "no.."]
