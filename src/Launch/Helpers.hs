module Launch.Helpers
  ( binaryPath,
    daemonize,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Language.Haskell.TH.Syntax as TH
import qualified System.Directory as Directory
import qualified System.Posix as Posix
import qualified System.Posix.Daemonize

daemonize :: IO () -> IO ()
daemonize io = do
  -- If this Haskell process dies the terminal it runs it will exit. If that
  -- happens while daemonization is ongoing, that daemonization might fail.
  -- We use a semaphore to wait for daemonization to finish, before exiting the
  -- process.
  sem <- Posix.semOpen "launch-daemon" (Posix.OpenSemFlags True False) Posix.ownerModes 0
  -- One step in daemonizing a Unix process is to exit the parent process before
  -- the child process proper gets going. We need to keep our root haskell
  -- process running though, to keep the terminal around for a bit (see comment
  -- above). To that end we create a child process here that can be exited
  -- during daemonization.
  _ <-
    Posix.forkProcess $
      System.Posix.Daemonize.daemonize $ do Posix.semPost sem; io
  -- Wait for daemonization to complete.
  Posix.semWait sem

binaryPath :: String -> TH.Q TH.Exp
binaryPath name = do
  maybeBin <- liftIO (Directory.findExecutable name)
  case maybeBin of
    Nothing -> error "Cannot find wtype binary"
    Just bin -> TH.lift bin
