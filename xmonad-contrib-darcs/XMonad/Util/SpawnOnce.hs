{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.SpawnOnce
-- Copyright   :  (c) Spencer Janssen 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  spencerjanssen@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- A module for spawning a command once, and only once.  Useful to start
-- status bars and make session settings inside startupHook.
--
-----------------------------------------------------------------------------

module XMonad.Util.SpawnOnce (spawnOnce) where

import XMonad
import Data.Set as Set
import qualified XMonad.Util.ExtensibleState as XS
import Control.Monad

data SpawnOnce = SpawnOnce { unspawnOnce :: (Set String) }
    deriving (Read, Show, Typeable)

instance ExtensionClass SpawnOnce where
    initialValue = SpawnOnce Set.empty
    extensionType = PersistentExtension

-- | The first time 'spawnOnce' is executed on a particular command, that
-- command is executed.  Subsequent invocations for a command do nothing.
spawnOnce :: String -> X ()
spawnOnce xs = do
    b <- XS.gets (Set.member xs . unspawnOnce)
    when (not b) $ do
        spawn xs
        XS.modify (SpawnOnce . Set.insert xs . unspawnOnce)
