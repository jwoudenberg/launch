module Launch (main) where

import Conduit
import qualified Control.Concurrent
import qualified Data.Attoparsec.Text as P
import qualified Data.ByteString as B
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Process as C.Process
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit
import qualified System.FilePath as FilePath
import qualified System.Posix.Daemonize
import qualified System.Posix.Process
import qualified System.Process as Process
import qualified Text.Emoji

main :: IO ()
main =
  runResourceT $ do
    (c, (), ()) <-
      C.Process.sourceProcessWithStreams
        fzf
        fzfStdin
        fzfStdout
        stderrC
    liftIO $ System.Exit.exitWith c

data FzfOption = FzfOption
  { description :: Builder.Builder,
    payload :: Builder.Builder,
    action :: Action
  }

data Action
  = LaunchApplication
  | InsertEmoji

toFzfStdinLine :: FzfOption -> Builder.Builder
toFzfStdinLine option =
  fzfOptionType (action option)
    <> "\FS"
    <> payload option
    <> "\FS"
    <> description option

fzfOptionType :: Action -> Builder.Builder
fzfOptionType action' =
  case action' of
    LaunchApplication -> "launch"
    InsertEmoji -> "emoji"

runAction :: Action -> T.Text -> IO ()
runAction action payload =
  case action of
    LaunchApplication ->
      case T.unpack <$> T.words payload of
        [] -> pure ()
        (file : args) ->
          daemonize $ System.Posix.Process.executeFile file True args Nothing
    InsertEmoji ->
      -- `wl-copy` spawns a long-running process. If we don't daemonize here
      -- then `launch` will hang instead of exiting.
      daemonize $ do
        -- Copy the selected emoji to the clipboard. Ideally I'd like to insert
        -- it directly, but I don't know how to do so yet in wayland. ydotools
        -- seems capable of sending input to text, but it requires sudo
        -- priviliges to run. There also seems to be some discussion on wayland
        -- virtual keyboards, but I haven't done a deep-dive into that yet.
        _ <-
          Process.readProcess
            "wl-copy"
            []
            (T.unpack payload)
        pure ()

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
  Control.Concurrent.threadDelay 10_000 {- 10 ms -}

parseFzfLine :: T.Text -> Maybe (Action, T.Text)
parseFzfLine line =
  case T.splitOn "\FS" line of
    ["launch", exec, _] -> Just (LaunchApplication, exec)
    ["emoji", emoji', _] -> Just (InsertEmoji, emoji')
    _ -> Nothing

fzf :: Process.CreateProcess
fzf =
  Process.proc
    "fzf"
    [ "--no-sort",
      "--delimiter=\FS",
      "--with-nth=3",
      "--no-info"
    ]

fzfStdin :: MonadResource m => ConduitT i B.ByteString m ()
fzfStdin =
  fzfOptions
    .| mapC (TL.toStrict . Builder.toLazyText . toFzfStdinLine)
    .| unlinesC
    .| encodeUtf8C

fzfOptions :: MonadResource m => ConduitT i FzfOption m ()
fzfOptions = do
  applications
  emoji

applications :: MonadResource m => ConduitT i FzfOption m ()
applications =
  yieldM (liftIO (Environment.getEnv "XDG_DATA_DIRS"))
    .| C.splitOnUnboundedE (== ':')
    .| mapC (FilePath.</> "applications")
    .| filterMC (liftIO . Directory.doesDirectoryExist)
    .| awaitForever sourceDirectory
    .| filterC (FilePath.isExtensionOf ".desktop")
    .| awaitForever sourceFile
    .| concatMapC TE.decodeUtf8'
    .| mapC (P.parseOnly desktopFileParser)
    .| concatC
    .| mapC desktopFile
    .| concatC

emoji :: Monad m => ConduitT i FzfOption m ()
emoji =
  yieldMany Text.Emoji.emojis
    .| mapC (uncurry fzfEntryForEmoji)

fzfEntryForEmoji :: T.Text -> T.Text -> FzfOption
fzfEntryForEmoji alias emoji' =
  FzfOption
    { description = Builder.fromText emoji' <> " :" <> Builder.fromText alias <> ":",
      action = InsertEmoji,
      payload = Builder.fromText emoji'
    }

fzfStdout :: (MonadIO m, MonadThrow m) => ConduitT B.ByteString o m ()
fzfStdout =
  decodeUtf8C
    .| linesUnboundedC
    .| mapC parseFzfLine
    .| concatC
    .| mapM_C (liftIO . uncurry runAction)

desktopFile :: Map.Map T.Text Builder.Builder -> Maybe FzfOption
desktopFile pairs = do
  name <- Map.lookup "Name" pairs
  exec <- Map.lookup "Exec" pairs
  pure
    FzfOption
      { description = name,
        action = LaunchApplication,
        payload = exec
      }

desktopFileParser :: P.Parser (Map.Map T.Text Builder.Builder)
desktopFileParser = do
  _ <- P.string "[Desktop Entry]"
  _ <- P.many1 P.endOfLine
  keyValuePairs mempty

keyValuePairs :: Map.Map T.Text Builder.Builder -> P.Parser (Map.Map T.Text Builder.Builder)
keyValuePairs pairs = do
  key <- P.takeWhile1 (/= '=')
  _ <- P.char '='
  val <- value ""
  _ <- P.many1 P.endOfLine
  P.option pairs (keyValuePairs (Map.insert key val pairs))

value :: Builder.Builder -> P.Parser Builder.Builder
value acc = do
  bit <- P.takeTill (\c -> P.isEndOfLine c || c == '%')
  let newAcc = acc <> (Builder.fromText bit)
  P.choice
    [ P.string "%f" *> value newAcc,
      P.string "%F" *> value newAcc,
      P.string "%u" *> value newAcc,
      P.string "%U" *> value newAcc,
      P.string "%d" *> value newAcc,
      P.string "%D" *> value newAcc,
      P.string "%n" *> value newAcc,
      P.string "%N" *> value newAcc,
      P.string "%i" *> value newAcc,
      P.string "%c" *> value newAcc,
      P.string "%k" *> value newAcc,
      P.string "%v" *> value newAcc,
      P.string "%m" *> value newAcc,
      P.char '%' *> value (newAcc <> "%"),
      pure newAcc
    ]
