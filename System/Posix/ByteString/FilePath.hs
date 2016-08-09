module System.Posix.ByteString.FilePath(
    RawFilePath, withFilePath, peekFilePath, peekFilePathLen,
    throwErrnoPathIfMinus1Retry,
    throwErrnoPathIfMinus1Retry_,
    throwErrnoPathIfNullRetry,
    throwErrnoPathIfRetry,
    throwErrnoPath,
    throwErrnoPathIf,
    throwErrnoPathIf_,
    throwErrnoPathIfNull,
    throwErrnoPathIfMinus1,
    throwErrnoPathIfMinus1_
) where

import Prelude hiding (FilePath)
import FilePath.ByteString
import System.Posix.FsUtils.ByteString

type RawFilePath = FilePath
