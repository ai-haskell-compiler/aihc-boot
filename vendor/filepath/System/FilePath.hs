{- |
Module      :  System.FilePath
Copyright   :  (c) Neil Mitchell 2005-2014
License     :  BSD3

Maintainer  :  ndmitchell@gmail.com
Stability   :  stable
Portability :  portable

A library for 'FilePath' manipulations, using Posix or Windows filepaths
depending on the platform.

aihc-boot supports POSIX paths only, so this module re-exports
"System.FilePath.Posix".
-}
module System.FilePath(
    -- * Separator predicates
    FilePath,

    -- * Directory functions
    takeDirectory,

    -- * File name manipulations
    combine, (</>),
) where

import System.FilePath.Posix
