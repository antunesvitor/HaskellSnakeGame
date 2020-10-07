{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_SnakeGame (
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

bindir     = "/home/vitor/Documents/Projects/SnakeGame/.stack-work/install/x86_64-linux-tinfo6/56fdca7a8e946742d757ef12302e8aa01b01deea67f366ad154de1e54d45c9c3/8.8.4/bin"
libdir     = "/home/vitor/Documents/Projects/SnakeGame/.stack-work/install/x86_64-linux-tinfo6/56fdca7a8e946742d757ef12302e8aa01b01deea67f366ad154de1e54d45c9c3/8.8.4/lib/x86_64-linux-ghc-8.8.4/SnakeGame-0.1.0.0-8Vpt2tNix2nFIHPz9ircHN-SnakeGame"
dynlibdir  = "/home/vitor/Documents/Projects/SnakeGame/.stack-work/install/x86_64-linux-tinfo6/56fdca7a8e946742d757ef12302e8aa01b01deea67f366ad154de1e54d45c9c3/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/vitor/Documents/Projects/SnakeGame/.stack-work/install/x86_64-linux-tinfo6/56fdca7a8e946742d757ef12302e8aa01b01deea67f366ad154de1e54d45c9c3/8.8.4/share/x86_64-linux-ghc-8.8.4/SnakeGame-0.1.0.0"
libexecdir = "/home/vitor/Documents/Projects/SnakeGame/.stack-work/install/x86_64-linux-tinfo6/56fdca7a8e946742d757ef12302e8aa01b01deea67f366ad154de1e54d45c9c3/8.8.4/libexec/x86_64-linux-ghc-8.8.4/SnakeGame-0.1.0.0"
sysconfdir = "/home/vitor/Documents/Projects/SnakeGame/.stack-work/install/x86_64-linux-tinfo6/56fdca7a8e946742d757ef12302e8aa01b01deea67f366ad154de1e54d45c9c3/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "SnakeGame_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SnakeGame_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "SnakeGame_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "SnakeGame_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SnakeGame_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "SnakeGame_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
