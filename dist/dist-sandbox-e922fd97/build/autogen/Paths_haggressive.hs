module Paths_haggressive (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/pold/Dropbox/Uni/Radboud/Text_Mining/Haggressive/.cabal-sandbox/bin"
libdir     = "/home/pold/Dropbox/Uni/Radboud/Text_Mining/Haggressive/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.3/haggressive-0.1.0.0"
datadir    = "/home/pold/Dropbox/Uni/Radboud/Text_Mining/Haggressive/.cabal-sandbox/share/x86_64-linux-ghc-7.8.3/haggressive-0.1.0.0"
libexecdir = "/home/pold/Dropbox/Uni/Radboud/Text_Mining/Haggressive/.cabal-sandbox/libexec"
sysconfdir = "/home/pold/Dropbox/Uni/Radboud/Text_Mining/Haggressive/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haggressive_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haggressive_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "haggressive_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haggressive_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haggressive_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
