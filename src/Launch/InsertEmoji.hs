module Launch.InsertEmoji (options, exec) where

import Conduit
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as Builder
import qualified Launch.Fzf as Fzf
import qualified System.Process as Process
import qualified Text.Emoji

options :: Monad m => ConduitT i Fzf.Option m ()
options =
  yieldMany Text.Emoji.emojis
    .| mapC (uncurry fzfEntryForEmoji)

fzfEntryForEmoji :: T.Text -> T.Text -> Fzf.Option
fzfEntryForEmoji alias emoji =
  Fzf.Option
    { Fzf.description = Builder.fromText emoji <> " :" <> Builder.fromText alias <> ":",
      Fzf.action = Fzf.InsertEmoji,
      Fzf.payload = Builder.fromText emoji
    }

exec :: T.Text -> IO ()
exec emoji =
  -- `wl-copy` spawns a long-running process. If we don't daemonize here
  -- then `launch` will hang instead of exiting.
  Fzf.daemonize $ do
    -- Copy the selected emoji to the clipboard. Ideally I'd like to insert
    -- it directly, but I don't know how to do so yet in wayland. ydotools
    -- seems capable of sending input to text, but it requires sudo
    -- priviliges to run. There also seems to be some discussion on wayland
    -- virtual keyboards, but I haven't done a deep-dive into that yet.
    _ <-
      Process.readProcess
        "wl-copy"
        []
        (T.unpack emoji)
    pure ()
