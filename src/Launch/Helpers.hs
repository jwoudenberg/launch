module Launch.Helpers
  ( binaryPath,
    daemonize,
  )
where

import qualified Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import qualified Language.Haskell.TH.Syntax as TH
import qualified System.Directory as Directory
import qualified System.Posix.Daemonize
import qualified System.Posix.Process

daemonize :: IO () -> IO ()
daemonize io = do
  _ <-
    System.Posix.Process.forkProcess $
      -- `daemonize` will exit the current process. We run it in a
      -- `forkProcess` so the launcher process itself isn't quit just yet.
      System.Posix.Daemonize.daemonize $ io
  Control.Concurrent.threadDelay 20_000

binaryPath :: String -> TH.Q TH.Exp
binaryPath name = do
  maybeBin <- liftIO (Directory.findExecutable name)
  case maybeBin of
    Nothing -> error "Cannot find wtype binary"
    Just bin -> TH.lift bin
