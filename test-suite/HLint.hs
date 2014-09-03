{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : MPL-2.0
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  test-suite/HLint.hs
-}

module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "systemk"
    , "src"
    , "test-suite"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure
