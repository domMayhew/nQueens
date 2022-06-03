{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_tests (
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
version = Version [0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Dominic\\OneDrive\\Documents\\prog\\Haskell\\.stack-work\\install\\f9169b82\\bin"
libdir     = "C:\\Users\\Dominic\\OneDrive\\Documents\\prog\\Haskell\\.stack-work\\install\\f9169b82\\lib\\x86_64-windows-ghc-8.10.7\\tests-0.0.1-LBTW5qBzRQ2DsXEds4iCDF"
dynlibdir  = "C:\\Users\\Dominic\\OneDrive\\Documents\\prog\\Haskell\\.stack-work\\install\\f9169b82\\lib\\x86_64-windows-ghc-8.10.7"
datadir    = "C:\\Users\\Dominic\\OneDrive\\Documents\\prog\\Haskell\\.stack-work\\install\\f9169b82\\share\\x86_64-windows-ghc-8.10.7\\tests-0.0.1"
libexecdir = "C:\\Users\\Dominic\\OneDrive\\Documents\\prog\\Haskell\\.stack-work\\install\\f9169b82\\libexec\\x86_64-windows-ghc-8.10.7\\tests-0.0.1"
sysconfdir = "C:\\Users\\Dominic\\OneDrive\\Documents\\prog\\Haskell\\.stack-work\\install\\f9169b82\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tests_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tests_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "tests_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "tests_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tests_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tests_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
