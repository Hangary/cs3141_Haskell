{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Hare (
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
version = Version [1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Hangar/Projects/COMP3141/Hare/.stack-work/install/x86_64-osx/c1192501630167f39d3fd57b1aa39dd46cd68caa04c4a13f8ee2df9873f22c29/8.0.2/bin"
libdir     = "/Users/Hangar/Projects/COMP3141/Hare/.stack-work/install/x86_64-osx/c1192501630167f39d3fd57b1aa39dd46cd68caa04c4a13f8ee2df9873f22c29/8.0.2/lib/x86_64-osx-ghc-8.0.2/Hare-1.0"
dynlibdir  = "/Users/Hangar/Projects/COMP3141/Hare/.stack-work/install/x86_64-osx/c1192501630167f39d3fd57b1aa39dd46cd68caa04c4a13f8ee2df9873f22c29/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/Hangar/Projects/COMP3141/Hare/.stack-work/install/x86_64-osx/c1192501630167f39d3fd57b1aa39dd46cd68caa04c4a13f8ee2df9873f22c29/8.0.2/share/x86_64-osx-ghc-8.0.2/Hare-1.0"
libexecdir = "/Users/Hangar/Projects/COMP3141/Hare/.stack-work/install/x86_64-osx/c1192501630167f39d3fd57b1aa39dd46cd68caa04c4a13f8ee2df9873f22c29/8.0.2/libexec"
sysconfdir = "/Users/Hangar/Projects/COMP3141/Hare/.stack-work/install/x86_64-osx/c1192501630167f39d3fd57b1aa39dd46cd68caa04c4a13f8ee2df9873f22c29/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Hare_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Hare_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Hare_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Hare_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Hare_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Hare_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
