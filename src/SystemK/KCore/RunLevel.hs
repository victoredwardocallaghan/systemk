{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : MPL-2.0
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  Runlevels define the mode of operation of the pure state-machine
  specified herein. The runlevels modelled here are based on the
  SVR4/Solaris 'standard'.
-}

{-# LANGUAGE Safe #-}
module SystemK.KCore.RunLevel where

-- | RunLevels are states defined fall into the following five
--   categories:
--   * signal-user mode
--   * multi-user mode without network services started
--   * multi-user mode with    network services started
--   * system shutdown
--   * system reboot
--
data RunLevel = State0 -- ^ Operating system halts

              | StateS -- ^ Single-user mode with only the
                       --   root filesystem mounted read-only.

              | State1 -- ^ Single-user mode with all local
                       --   filesystems mounted read-write.

              | State2 -- ^ Multi-user mode with all non-network
                       --   services started.

              | State3 -- ^ Multi-user mode identical to @State2@
                       --   with the addition of network started.

              | State4 -- ^ Alternative to @State2@, user-definable.

              | State5 -- ^ Shutdown, halts and poweroff the hardware.

              | State6 -- ^ Reboot, halts and reboots the hardware.

                deriving (Ord,Eq)
