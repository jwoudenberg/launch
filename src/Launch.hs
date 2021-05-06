module Launch (main) where

import Conduit
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
import qualified System.Process as Process

main :: IO ()
main =
  runResourceT $ do
    (c, cmd, ()) <-
      C.Process.sourceProcessWithStreams
        fzf
        fzfStdin
        fzfStdout
        stderrC
    liftIO $ do
      maybeExit c
      run cmd

run :: T.Text -> IO ()
run cmd =
  case T.unpack <$> T.words cmd of
    [] -> pure ()
    (file : args) ->
      System.Posix.Daemonize.daemonize $ do
        _ <- Process.spawnProcess file args
        pure ()

maybeExit :: System.Exit.ExitCode -> IO ()
maybeExit code =
  case code of
    System.Exit.ExitSuccess -> pure ()
    System.Exit.ExitFailure _ -> System.Exit.exitWith code

fzf :: Process.CreateProcess
fzf =
  Process.proc
    "fzf"
    [ "--no-sort",
      "--delimiter=\FS",
      "--with-nth=2",
      "--no-info"
    ]

fzfStdin :: MonadResource m => ConduitT i B.ByteString m ()
fzfStdin =
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
    .| unlinesC
    .| encodeUtf8C

fzfStdout :: MonadThrow m => ConduitT B.ByteString o m T.Text
fzfStdout =
  decodeUtf8C
    .| takeWhileCE (/= '\FS')
    .| foldC

desktopFile :: Map.Map T.Text Builder.Builder -> Maybe T.Text
desktopFile pairs = do
  name <- Map.lookup "Name" pairs
  exec <- Map.lookup "Exec" pairs
  pure (TL.toStrict (Builder.toLazyText (exec <> "\FS" <> name)))

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
