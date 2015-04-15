module Paths_xmonad_contrib (
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
version = Version [0,12] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/jeffrey/.cabal/bin"
libdir     = "/home/jeffrey/.cabal/lib/x86_64-linux-ghc-7.10.1/xmona_JCpKcgGshcHLwzx6lZoAFQ"
datadir    = "/home/jeffrey/.cabal/share/x86_64-linux-ghc-7.10.1/xmonad-contrib-0.12"
libexecdir = "/home/jeffrey/.cabal/libexec"
sysconfdir = "/home/jeffrey/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "xmonad_contrib_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "xmonad_contrib_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "xmonad_contrib_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xmonad_contrib_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "xmonad_contrib_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
