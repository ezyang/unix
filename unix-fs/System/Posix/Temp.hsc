{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE OverloadedStrings #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Temp
-- Copyright   :  (c) Volker Stolz <vs@foldr.org>
--                    Deian Stefan <deian@cs.stanford.edu>
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org, vs@foldr.org, deian@cs.stanford.edu
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX temporary file and directory creation functions.
--
-----------------------------------------------------------------------------

module System.Posix.Temp (
        mkstemp, mkstemps, mkdtemp
    ) where

#include "HsUnix.h"

import Prelude hiding (FilePath)

import Foreign.C
import System.IO hiding (FilePath)
import System.Posix.Directory (createDirectory)
import System.Posix.IO
import System.Posix.Types

import FilePath

have_mkdtemp :: Bool
#if HAVE_MKDTEMP
have_mkdtemp = True
#else
have_mkdtemp = False
#endif

have_mkstemps :: Bool
#if HAVE_MKSTEMPS
have_mkstemps = True
#else
have_mkstemps = False
#endif

foreign import capi unsafe "HsUnix.h mkstemp"
  c_mkstemp :: CString -> IO CInt

-- | Make a unique filename and open it for reading\/writing. The returned
-- 'FilePath' is the (possibly relative) path of the created file, which is
-- padded with 6 random characters. The argument is the desired prefix of the
-- filepath of the temporary file to be created.
--
-- If you aren't using GHC or Hugs then this function simply wraps mktemp and
-- so shouldn't be considered safe.
mkstemp :: FilePath -> IO (FilePath, Handle)
mkstemp template' = do
  let template = template' `mappend` "XXXXXX"
  withFilePath template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemp" (c_mkstemp ptr)
    name <- peekFilePath ptr
    h <- fdToHandle (Fd fd)
    return (name, h)

#if HAVE_MKSTEMPS
foreign import capi unsafe "HsUnix.h mkstemps"
  c_mkstemps :: CString -> CInt -> IO CInt
#else
c_mkstemps :: CString -> CInt -> IO CInt
c_mkstemps = error "c_mkstemps: !HAVE_MKSTEMPS"
#endif

-- | Make a unique filename with a given prefix and suffix and open it for
-- reading\/writing. The returned 'FilePath' is the (possibly relative) path of
-- the created file, which contains  6 random characters in between the prefix
-- and suffix. The first argument is the desired prefix of the filepath of the
-- temporary file to be created. The second argument is the suffix of the
-- temporary file to be created.
--
-- If you are using as system that doesn't support the mkstemps glibc function
-- (supported in glibc > 2.11) then this function simply throws an error.
mkstemps :: FilePath -> FilePath -> IO (FilePath, Handle)
mkstemps prefix suffix
 | have_mkstemps = do
  let template = prefix `mappend` "XXXXXX" `mappend` suffix
      lenOfsuf = (fromIntegral $ lengthFilePath suffix) :: CInt
  withFilePath template $ \ ptr -> do
    fd <- throwErrnoIfMinus1 "mkstemps" (c_mkstemps ptr lenOfsuf)
    name <- peekFilePath ptr
    h <- fdToHandle (Fd fd)
    return (name, h)
 | otherwise = error "System.Posix.Temp.mkstemps: not available on this platform"

#if HAVE_MKDTEMP
foreign import capi unsafe "HsUnix.h mkdtemp"
  c_mkdtemp :: CString -> IO CString
#else
c_mkdtemp :: CString -> IO CString
c_mkdtemp = error "c_mkdtemp: !HAVE_MKDTEMP"
#endif

-- | Make a unique directory. The returned 'FilePath' is the path of the
-- created directory, which is padded with 6 random characters. The argument is
-- the desired prefix of the filepath of the temporary directory to be created.
--
-- If you are using as system that doesn't support the mkdtemp glibc function
-- (supported in glibc > 2.1.91) then this function uses mktemp and so
-- shouldn't be considered safe.
mkdtemp :: FilePath -> IO FilePath
mkdtemp template'
 | have_mkdtemp = do
  withFilePath template $ \ ptr -> do
    _ <- throwErrnoIfNull "mkdtemp" (c_mkdtemp ptr)
    name <- peekFilePath ptr
    return name
 | otherwise = do
  name <- mktemp template
  _ <- createDirectory name (toEnum 0o700)
  return name
 where
  template = template' `mappend` "XXXXXX"

#if !HAVE_MKDTEMP
foreign import ccall unsafe "mktemp"
  c_mktemp :: CString -> IO CString
#else
c_mktemp :: CString -> IO CString
c_mktemp = error "c_mktemp: HAVE_MKDTEMP"
#endif

-- | Make a unique file name It is required that the template have six trailing
-- \'X\'s. This function should be considered deprecated.
{-# WARNING mktemp "This function is unsafe; use mkstemp instead" #-}
mktemp :: FilePath -> IO FilePath
mktemp template = do
  withFilePath template $ \t_ptr -> do
    ptr <- throwErrnoIfNull "mktemp" (c_mktemp t_ptr)
    peekFilePath ptr
