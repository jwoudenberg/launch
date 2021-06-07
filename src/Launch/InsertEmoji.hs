module Launch.InsertEmoji (Option (..), options, exec) where

import Conduit
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as Builder
import qualified Launch.Helpers as Helpers
import qualified System.Posix.Process
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
  Helpers.daemonize $
    -- Daemonize, then after a small deplay insert, so the terminal this is
    -- running in can close and the insert will happen into the window with
    -- focus.
    System.Posix.Process.executeFile
      "wtype"
      True
      ["-s", "100", T.unpack emoji']
      Nothing
