module Launch.InsertEmoji (Option (..), options, exec) where

import Conduit
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as Builder
import qualified Launch.Helpers as Helpers
import qualified System.Process as Process
import qualified Text.Emoji

data Option = Option
  { emoji :: Builder.Builder,
    description :: Builder.Builder
  }

options :: Monad m => ConduitT i Option m ()
options =
  yieldMany Text.Emoji.emojis
    .| mapC (uncurry fzfEntryForEmoji)

fzfEntryForEmoji :: T.Text -> T.Text -> Option
fzfEntryForEmoji alias emoji =
  Option
    { description = Builder.fromText emoji <> " :" <> Builder.fromText alias <> ":",
      emoji = Builder.fromText emoji
    }

exec :: T.Text -> IO ()
exec emoji' =
  -- `wl-copy` spawns a long-running process. If we don't daemonize here
  -- then `launch` will hang instead of exiting.
  Helpers.daemonize $ do
    -- Copy the selected emoji to the clipboard. Ideally I'd like to insert
    -- it directly, but I don't know how to do so yet in wayland. ydotools
    -- seems capable of sending input to text, but it requires sudo
    -- priviliges to run. There also seems to be some discussion on wayland
    -- virtual keyboards, but I haven't done a deep-dive into that yet.
    _ <-
      Process.readProcess
        "wl-copy"
        []
        (T.unpack emoji')
    pure ()
