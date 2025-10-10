{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_qlexers (
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
bindir     = "/Users/luisantonioguzmanbucio/Desktop/BSc. Mathematics/UNAM/7th Sem/Compilers/Proyecto01/qlexers/.stack-work/install/aarch64-osx/2aa2de78e47301f33daf32b61840d6730754062efdf8affebf31f0517bebcb52/9.6.6/bin"
libdir     = "/Users/luisantonioguzmanbucio/Desktop/BSc. Mathematics/UNAM/7th Sem/Compilers/Proyecto01/qlexers/.stack-work/install/aarch64-osx/2aa2de78e47301f33daf32b61840d6730754062efdf8affebf31f0517bebcb52/9.6.6/lib/aarch64-osx-ghc-9.6.6/qlexers-0.1.0.0-6rdR7YDqHapLxFHElxrZB5-qlexers"
dynlibdir  = "/Users/luisantonioguzmanbucio/Desktop/BSc. Mathematics/UNAM/7th Sem/Compilers/Proyecto01/qlexers/.stack-work/install/aarch64-osx/2aa2de78e47301f33daf32b61840d6730754062efdf8affebf31f0517bebcb52/9.6.6/lib/aarch64-osx-ghc-9.6.6"
datadir    = "/Users/luisantonioguzmanbucio/Desktop/BSc. Mathematics/UNAM/7th Sem/Compilers/Proyecto01/qlexers/.stack-work/install/aarch64-osx/2aa2de78e47301f33daf32b61840d6730754062efdf8affebf31f0517bebcb52/9.6.6/share/aarch64-osx-ghc-9.6.6/qlexers-0.1.0.0"
libexecdir = "/Users/luisantonioguzmanbucio/Desktop/BSc. Mathematics/UNAM/7th Sem/Compilers/Proyecto01/qlexers/.stack-work/install/aarch64-osx/2aa2de78e47301f33daf32b61840d6730754062efdf8affebf31f0517bebcb52/9.6.6/libexec/aarch64-osx-ghc-9.6.6/qlexers-0.1.0.0"
sysconfdir = "/Users/luisantonioguzmanbucio/Desktop/BSc. Mathematics/UNAM/7th Sem/Compilers/Proyecto01/qlexers/.stack-work/install/aarch64-osx/2aa2de78e47301f33daf32b61840d6730754062efdf8affebf31f0517bebcb52/9.6.6/etc"

getBinDir     = catchIO (getEnv "qlexers_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "qlexers_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "qlexers_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "qlexers_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "qlexers_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "qlexers_sysconfdir") (\_ -> return sysconfdir)



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
