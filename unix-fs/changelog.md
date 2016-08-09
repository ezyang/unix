# Changelog for [`unix-fs` package](http://hackage.haskell.org/package/unix-fs)

## HEAD

  * Converted to Backpack, now requires GHC 8.1 or later

  * `withModule` in `System.Posix.DynamicLinker.Module` now uses
    `</>` to append the directory and file path.  Previously,
    if the file path had a leading slash, it was still treated
    relatively.
