# Changelog for [`unix-process` package](http://hackage.haskell.org/package/unix-process)

## HEAD

  * Converted to Backpack, now requires GHC 8.1 or later

  * `executeFile` now shares the string of the path of the
    executable being executed between the first and second
    arguments of `execv` and `execvp`.
