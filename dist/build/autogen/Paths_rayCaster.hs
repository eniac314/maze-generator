module Paths_rayCaster (
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

bindir     = "/home/fayong/prog/Haskell/rayCaster/.cabal-sandbox/bin"
libdir     = "/home/fayong/prog/Haskell/rayCaster/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.2/rayCaster-0.1.0.0-78vXd54C3fiGtRZWdxbOYS"
datadir    = "/home/fayong/prog/Haskell/rayCaster/.cabal-sandbox/share/x86_64-linux-ghc-7.10.2/rayCaster-0.1.0.0"
libexecdir = "/home/fayong/prog/Haskell/rayCaster/.cabal-sandbox/libexec"
sysconfdir = "/home/fayong/prog/Haskell/rayCaster/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "rayCaster_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "rayCaster_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "rayCaster_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rayCaster_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "rayCaster_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
