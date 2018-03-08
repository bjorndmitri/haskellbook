{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_morse (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/samuelwalls/projects/study/haskell/haskellbook/morse/.stack-work/install/x86_64-osx/lts-9.18/8.0.2/bin"
libdir     = "/Users/samuelwalls/projects/study/haskell/haskellbook/morse/.stack-work/install/x86_64-osx/lts-9.18/8.0.2/lib/x86_64-osx-ghc-8.0.2/morse-0.1.0.0-EoGXyWdC3F14WkTSMAgy15"
dynlibdir  = "/Users/samuelwalls/projects/study/haskell/haskellbook/morse/.stack-work/install/x86_64-osx/lts-9.18/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/samuelwalls/projects/study/haskell/haskellbook/morse/.stack-work/install/x86_64-osx/lts-9.18/8.0.2/share/x86_64-osx-ghc-8.0.2/morse-0.1.0.0"
libexecdir = "/Users/samuelwalls/projects/study/haskell/haskellbook/morse/.stack-work/install/x86_64-osx/lts-9.18/8.0.2/libexec"
sysconfdir = "/Users/samuelwalls/projects/study/haskell/haskellbook/morse/.stack-work/install/x86_64-osx/lts-9.18/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "morse_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "morse_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "morse_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "morse_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "morse_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "morse_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
