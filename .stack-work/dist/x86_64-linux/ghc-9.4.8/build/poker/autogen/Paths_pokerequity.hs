{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_pokerequity (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/brendan/pokerequity/.stack-work/install/x86_64-linux/8847e3ed53caa59c413a3bb61c63a2894cb497dd77063ab666864dfcb42c828f/9.4.8/bin"
libdir     = "/home/brendan/pokerequity/.stack-work/install/x86_64-linux/8847e3ed53caa59c413a3bb61c63a2894cb497dd77063ab666864dfcb42c828f/9.4.8/lib/x86_64-linux-ghc-9.4.8/pokerequity-0.1.0.0-FcpO3VRvLMt1swXKLxyFHL-poker"
dynlibdir  = "/home/brendan/pokerequity/.stack-work/install/x86_64-linux/8847e3ed53caa59c413a3bb61c63a2894cb497dd77063ab666864dfcb42c828f/9.4.8/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/home/brendan/pokerequity/.stack-work/install/x86_64-linux/8847e3ed53caa59c413a3bb61c63a2894cb497dd77063ab666864dfcb42c828f/9.4.8/share/x86_64-linux-ghc-9.4.8/pokerequity-0.1.0.0"
libexecdir = "/home/brendan/pokerequity/.stack-work/install/x86_64-linux/8847e3ed53caa59c413a3bb61c63a2894cb497dd77063ab666864dfcb42c828f/9.4.8/libexec/x86_64-linux-ghc-9.4.8/pokerequity-0.1.0.0"
sysconfdir = "/home/brendan/pokerequity/.stack-work/install/x86_64-linux/8847e3ed53caa59c413a3bb61c63a2894cb497dd77063ab666864dfcb42c828f/9.4.8/etc"

getBinDir     = catchIO (getEnv "pokerequity_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "pokerequity_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "pokerequity_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "pokerequity_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pokerequity_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pokerequity_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
