module Paths_Term (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/rei/.cabal/bin"
libdir     = "/home/rei/.cabal/lib/Term-0.1.0.0/ghc-7.6.3"
datadir    = "/home/rei/.cabal/share/Term-0.1.0.0"
libexecdir = "/home/rei/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Term_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Term_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Term_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Term_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
