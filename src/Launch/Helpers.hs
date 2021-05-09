module Launch.Helpers (daemonize) where

import qualified System.Posix.Daemonize
import qualified System.Posix.Process

daemonize :: IO () -> IO ()
daemonize io = do
  _ <-
    System.Posix.Process.forkProcess $
      -- `daemonize` will exit the current process. We run it in a
      -- `forkProcess` so the launcher process itself isn't quit just yet.
      System.Posix.Daemonize.daemonize $ io
  Prelude.pure ()
