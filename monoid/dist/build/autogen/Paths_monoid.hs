module Paths_monoid (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/samuelwalls/Library/Haskell/bin"
libdir     = "/Users/samuelwalls/Library/Haskell/ghc-7.10.3-x86_64/lib/monoid-0.1.0.0"
datadir    = "/Users/samuelwalls/Library/Haskell/share/ghc-7.10.3-x86_64/monoid-0.1.0.0"
libexecdir = "/Users/samuelwalls/Library/Haskell/libexec"
sysconfdir = "/Users/samuelwalls/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "monoid_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "monoid_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "monoid_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "monoid_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "monoid_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
