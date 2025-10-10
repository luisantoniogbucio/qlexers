{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_qlexers (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "qlexers"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Lexical Analyzer for IMP language"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
