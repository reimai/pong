module Paths_Pong (
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
libdir     = "/home/rei/.cabal/lib/Pong-0.1.0.0/ghc-7.6.3"
datadir    = "/home/rei/.cabal/share/Pong-0.1.0.0"
libexecdir = "/home/rei/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Pong_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Pong_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Pong_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Pong_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
