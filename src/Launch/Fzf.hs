module Launch.Fzf
  ( run,
    Option (..),
    Action (..),
    daemonize,
  )
where

import Conduit
import qualified Control.Concurrent
import qualified Data.ByteString as B
import qualified Data.Conduit.Process as C.Process
import qualified Data.Text.Lazy.Builder as Builder
import qualified System.Exit
import qualified System.Posix.Daemonize
import qualified System.Posix.Process
import qualified System.Process as Process

data Option = Option
  { description :: Builder.Builder,
    payload :: Builder.Builder,
    action :: Action
  }

data Action
  = LaunchApplication
  | InsertEmoji
  deriving (Bounded, Enum)

run ::
  MonadUnliftIO m =>
  ConduitT () B.ByteString m () ->
  ConduitT B.ByteString Void m () ->
  m ()
run stdin stdout = do
  (c, (), ()) <-
    C.Process.sourceProcessWithStreams
      proc
      stdin
      stdout
      stderrC
  liftIO $ System.Exit.exitWith c

proc :: Process.CreateProcess
proc =
  Process.proc
    "fzf"
    [ "--no-sort",
      "--delimiter=\FS",
      "--with-nth=3",
      "--no-info"
    ]

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
