-- | FilePath-parametric wrappers on functions from standard library.
module System.Posix.FsUtils (
     throwErrnoPathIfMinus1Retry,
     throwErrnoPathIfMinus1Retry_,
     throwErrnoPathIfNullRetry,
     throwErrnoPathIfRetry,
     throwErrnoPath,
     throwErrnoPathIf,
     throwErrnoPathIf_,
     throwErrnoPathIfNull,
     throwErrnoPathIfMinus1,
     throwErrnoPathIfMinus1_,
     ioeSetFileName,
) where

import Prelude hiding (FilePath)
import Foreign.Ptr
import qualified System.Posix.Error as E
import qualified System.IO.Error as E
import FilePath

throwErrnoPathIfMinus1Retry :: (Eq a, Num a)
                            => String -> FilePath -> IO a -> IO a
throwErrnoPathIfMinus1Retry loc path f =
  E.throwErrnoPathIfMinus1Retry loc (unpackFilePath path) f

throwErrnoPathIfMinus1Retry_ :: (Eq a, Num a)
                             => String -> FilePath -> IO a -> IO ()
throwErrnoPathIfMinus1Retry_ loc path f =
  E.throwErrnoPathIfMinus1Retry_ loc (unpackFilePath path) f

throwErrnoPathIfNullRetry :: String -> FilePath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNullRetry loc path f =
  E.throwErrnoPathIfNullRetry loc (unpackFilePath path) f

throwErrnoPathIfRetry :: (a -> Bool) -> String -> FilePath -> IO a -> IO a
throwErrnoPathIfRetry pr loc path f =
  E.throwErrnoPathIfRetry pr loc (unpackFilePath path) f

throwErrnoPath :: String -> FilePath -> IO a
throwErrnoPath loc path = E.throwErrnoPath loc (unpackFilePath path)

throwErrnoPathIf :: (a -> Bool) -> String -> FilePath -> IO a -> IO a
throwErrnoPathIf cond loc path f = E.throwErrnoPathIf cond loc (unpackFilePath path) f

throwErrnoPathIf_ :: (a -> Bool) -> String -> FilePath -> IO a -> IO ()
throwErrnoPathIf_ cond loc path f  = E.throwErrnoPathIf_ cond loc (unpackFilePath path) f

throwErrnoPathIfNull :: String -> FilePath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNull loc path f = E.throwErrnoPathIfNull loc (unpackFilePath path) f

throwErrnoPathIfMinus1 :: (Eq a, Num a) => String -> FilePath -> IO a -> IO a
throwErrnoPathIfMinus1 loc path f = E.throwErrnoPathIfMinus1 loc (unpackFilePath path) f

throwErrnoPathIfMinus1_ :: (Eq a, Num a) => String -> FilePath -> IO a -> IO ()
throwErrnoPathIfMinus1_ loc path f = E.throwErrnoPathIfMinus1_ loc (unpackFilePath path) f

ioeSetFileName :: IOError -> FilePath -> IOError
ioeSetFileName ioe path = E.ioeSetFileName ioe (unpackFilePath path)
