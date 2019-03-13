{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_shawty (
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

bindir     = "/home/noel/Documents/Haskell/projects/url-shortener/.stack-work/install/x86_64-linux/lts-13.12/8.6.4/bin"
libdir     = "/home/noel/Documents/Haskell/projects/url-shortener/.stack-work/install/x86_64-linux/lts-13.12/8.6.4/lib/x86_64-linux-ghc-8.6.4/shawty-0.1.0.0-Fvynizy1Usc5iibLHhs7I-shawty"
dynlibdir  = "/home/noel/Documents/Haskell/projects/url-shortener/.stack-work/install/x86_64-linux/lts-13.12/8.6.4/lib/x86_64-linux-ghc-8.6.4"
datadir    = "/home/noel/Documents/Haskell/projects/url-shortener/.stack-work/install/x86_64-linux/lts-13.12/8.6.4/share/x86_64-linux-ghc-8.6.4/shawty-0.1.0.0"
libexecdir = "/home/noel/Documents/Haskell/projects/url-shortener/.stack-work/install/x86_64-linux/lts-13.12/8.6.4/libexec/x86_64-linux-ghc-8.6.4/shawty-0.1.0.0"
sysconfdir = "/home/noel/Documents/Haskell/projects/url-shortener/.stack-work/install/x86_64-linux/lts-13.12/8.6.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "shawty_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "shawty_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "shawty_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "shawty_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "shawty_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "shawty_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
