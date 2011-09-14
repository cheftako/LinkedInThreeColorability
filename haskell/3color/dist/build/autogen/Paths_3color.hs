module Paths_3color (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/jkessler/.cabal/bin"
libdir     = "/home/jkessler/.cabal/lib/3color-0.1/ghc-7.0.2"
datadir    = "/home/jkessler/.cabal/share/3color-0.1"
libexecdir = "/home/jkessler/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "3color_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "3color_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "3color_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "3color_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
