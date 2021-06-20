module Launch.Helpers
  ( binaryPath,
    daemonize,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Language.Haskell.TH.Syntax as TH
import qualified System.Directory as Directory
import qualified System.Posix.Daemonize
import qualified System.Posix.Process
import qualified System.Posix.Signals as Signals

daemonize :: IO () -> IO ()
daemonize io = do
  -- The launcher process is intended to run inside a terminal application. Once
  -- `daemonize` is called to spawn whatever process the user wishes to launch
  -- the main thread will complete, causing the terminal to close. Upon closing
  -- the terminal will send a SIGHUP signal to its child processes, i.e. this
  -- launcher. If that SIGHUP arrives in the middle of the daemonization process
  -- then the launch might fail.
  --
  -- To avoid this we ignore SIGHUP signals from here on out. The main thread is
  -- going to finish soon anyway, so it's fine.
  _ <- Signals.installHandler Signals.sigHUP Signals.Ignore Nothing
  _ <-
    System.Posix.Process.forkProcess $
      -- `daemonize` will exit the current process. We run it in a
      -- `forkProcess` so the launcher process itself isn't quit just yet.
      System.Posix.Daemonize.daemonize $ io
  pure ()

binaryPath :: String -> TH.Q TH.Exp
binaryPath name = do
  maybeBin <- liftIO (Directory.findExecutable name)
  case maybeBin of
    Nothing -> error "Cannot find wtype binary"
    Just bin -> TH.lift bin
