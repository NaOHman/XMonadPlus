module Paths_xmonad (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,12], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/jeffrey/.cabal/bin"
libdir     = "/home/jeffrey/.cabal/lib/x86_64-linux-ghc-7.8.4/xmonad-0.12"
datadir    = "/home/jeffrey/.cabal/share/x86_64-linux-ghc-7.8.4/xmonad-0.12"
libexecdir = "/home/jeffrey/.cabal/libexec"
sysconfdir = "/home/jeffrey/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "xmonad_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "xmonad_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "xmonad_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xmonad_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "xmonad_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
