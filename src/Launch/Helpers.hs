module Launch.Helpers (daemonize) where

import qualified Control.Concurrent
import qualified System.Posix.Daemonize
import qualified System.Posix.Process

daemonize :: IO () -> IO ()
daemonize io = do
  _ <-
    System.Posix.Process.forkProcess $
      -- `daemonize` will exit the current process. We run it in a
      -- `forkProcess` so the launcher process itself isn't quit just yet.
      System.Posix.Daemonize.daemonize $ io
  -- We need to wait a tiny bit for the process spawning above to complete.
  -- Without this when we run the script in a terminal we notice the
  -- selected application does not launch.
  Control.Concurrent.threadDelay 100_000 {- 100 ms -}
