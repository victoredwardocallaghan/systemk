{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : MPL-2.0
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module encapsulates internal structures and functions to deal with
  generic critical faults that are lifted up into within the context of the
  particular sub-system where the fault occured. Upon panic within that
  sub-system context a callout is made here to grasefully prepare a fault
  report.
-}

{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module SystemK.Utils.Panic (panic) where

import Control.Exception as X
import Data.Typeable (Typeable)
import Data.Maybe (fromMaybe,listToMaybe)

import SystemK.Version

-- | Function to wrap in respective sub-systems so that point of failure
--   maybe reported back to the user in the form of a critical fault report.
panic :: String -> [String] -> a
panic panicLoc panicMsg = throw SystemKPanic { .. }

data SystemKPanic = SystemKPanic { panicLoc :: String
                                 , panicMsg :: [String]
                                 } deriving Typeable

instance Show SystemKPanic where
  show p = unlines $
    [ "You have encountered a problem in the SystemK implementation."
    , "***" ++ repLab
    , "*** Please file a bug report with the following details and what happened:"
    , ""
    , "%< --------------------------------------------------- "
    ] ++ rev ++
    [ locLab ++ panicLoc p
    , msgLab ++ fromMaybe "" (listToMaybe msgLines)
    ]
    ++ map (tabs ++) (drop 1 msgLines) ++
    [ "%< --------------------------------------------------- "
    ]
    where msgLab    = " Message: "
          revLab    = " Revision: "
          branchLab = " Branch: "
          dirtyLab  = " (non-committed files present during build)"
          locLab    = " Location: "
          repLab    = " Report to: https://github.com/victoredwardocallaghan/systemk/issues"
          tabs      = map (const ' ') msgLab

          msgLines  = panicMsg p

          rev | null commitHash = []
              | otherwise = [ revLab ++ commitHash
                            , branchLab ++ commitBranch ++ dirtyLab ]

instance Exception SystemKPanic
