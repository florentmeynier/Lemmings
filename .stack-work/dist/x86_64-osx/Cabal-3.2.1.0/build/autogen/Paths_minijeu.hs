{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_minijeu (
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

bindir     = "/Users/habibbouchenaki/Documents/M1/S2/PAF/paf-tme6-minijeu-master/.stack-work/install/x86_64-osx/ab9fab92f25c4bdef1a27a04a6780a3b6e9debc38a1fef9f94ee526dc9fb31f5/8.10.3/bin"
libdir     = "/Users/habibbouchenaki/Documents/M1/S2/PAF/paf-tme6-minijeu-master/.stack-work/install/x86_64-osx/ab9fab92f25c4bdef1a27a04a6780a3b6e9debc38a1fef9f94ee526dc9fb31f5/8.10.3/lib/x86_64-osx-ghc-8.10.3/minijeu-0.1.0.0-588e2406CpS9XZorgjJ1yM"
dynlibdir  = "/Users/habibbouchenaki/Documents/M1/S2/PAF/paf-tme6-minijeu-master/.stack-work/install/x86_64-osx/ab9fab92f25c4bdef1a27a04a6780a3b6e9debc38a1fef9f94ee526dc9fb31f5/8.10.3/lib/x86_64-osx-ghc-8.10.3"
datadir    = "/Users/habibbouchenaki/Documents/M1/S2/PAF/paf-tme6-minijeu-master/.stack-work/install/x86_64-osx/ab9fab92f25c4bdef1a27a04a6780a3b6e9debc38a1fef9f94ee526dc9fb31f5/8.10.3/share/x86_64-osx-ghc-8.10.3/minijeu-0.1.0.0"
libexecdir = "/Users/habibbouchenaki/Documents/M1/S2/PAF/paf-tme6-minijeu-master/.stack-work/install/x86_64-osx/ab9fab92f25c4bdef1a27a04a6780a3b6e9debc38a1fef9f94ee526dc9fb31f5/8.10.3/libexec/x86_64-osx-ghc-8.10.3/minijeu-0.1.0.0"
sysconfdir = "/Users/habibbouchenaki/Documents/M1/S2/PAF/paf-tme6-minijeu-master/.stack-work/install/x86_64-osx/ab9fab92f25c4bdef1a27a04a6780a3b6e9debc38a1fef9f94ee526dc9fb31f5/8.10.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "minijeu_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "minijeu_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "minijeu_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "minijeu_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "minijeu_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "minijeu_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
