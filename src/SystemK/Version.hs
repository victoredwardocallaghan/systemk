{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : MPL-2.0
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable
-}

{-# LANGUAGE Safe #-}
module SystemK.Version where

import qualified GitRev

-- | git commit long hash
commitHash :: String
commitHash = GitRev.hash

-- | git commit short hash
commitShortHash :: String
commitShortHash = take 7 GitRev.hash

-- | git branch built from
commitBranch :: String
commitBranch = GitRev.branch

-- | wether the git repository had non-committed files during compliation
commitDirty :: Bool
commitDirty = GitRev.dirty
