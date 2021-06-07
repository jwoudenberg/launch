{-# LANGUAGE TemplateHaskell #-}

module Launch (main) where

import Conduit
import qualified Data.ByteString as B
import qualified Data.Conduit.Process as C.Process
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Launch.Helpers as Helpers
import qualified Launch.InsertEmoji as InsertEmoji
import qualified Launch.LaunchApplication as LaunchApplication
import qualified System.Exit
import qualified System.Process as Process

main :: IO ()
main = runResourceT $ do
  (c, (), ()) <-
    C.Process.sourceProcessWithStreams
      fzf
      fzfStdin
      fzfStdout
      stderrC
  case c of
    System.Exit.ExitSuccess -> pure ()
    System.Exit.ExitFailure _ ->
      liftIO $ System.Exit.exitWith c

data Action
  = LaunchApplication
  | InsertEmoji
  deriving (Bounded, Enum)

toFzfStdinLine :: Action -> Builder.Builder -> Builder.Builder -> Builder.Builder
toFzfStdinLine action description payload =
  description
    <> "\FS"
    <> Builder.fromText (actionToText action)
    <> "\FS"
    <> payload

actionToText :: Action -> T.Text
actionToText LaunchApplication = "launch"
actionToText InsertEmoji = "emoji"

actionByText :: Map.Map T.Text Action
actionByText =
  Map.fromList $
    fmap
      (\action -> (actionToText action, action))
      [minBound .. maxBound]

runAction :: Action -> T.Text -> IO ()
runAction action payload = do
  case action of
    LaunchApplication -> LaunchApplication.exec payload
    InsertEmoji -> InsertEmoji.exec payload

parseFzfLine :: T.Text -> Maybe (Action, T.Text)
parseFzfLine line =
  case T.splitOn "\FS" line of
    [_, actionText, payload'] -> do
      action' <- Map.lookup actionText actionByText
      pure (action', payload')
    _ -> Nothing

fzf :: Process.CreateProcess
fzf =
  Process.proc
    $(Helpers.binaryPath "fzf")
    [ "--delimiter=\FS",
      "--with-nth=1",
      "--no-info",
      "--no-multi",
      "--prompt=",
      "--color=gutter:-1"
    ]

fzfStdin :: MonadResource m => ConduitT i B.ByteString m ()
fzfStdin =
  fzfOptions
    .| mapC (TL.toStrict . Builder.toLazyText)
    .| unlinesC
    .| encodeUtf8C

fzfOptions :: MonadResource m => ConduitT i Builder.Builder m ()
fzfOptions = do
  LaunchApplication.options
    .| mapC
      ( \option ->
          toFzfStdinLine
            LaunchApplication
            (LaunchApplication.description option)
            (LaunchApplication.cmd option)
      )
  InsertEmoji.options
    .| mapC
      ( \option ->
          toFzfStdinLine
            InsertEmoji
            (InsertEmoji.description option)
            (InsertEmoji.emoji option)
      )

fzfStdout :: (MonadIO m, MonadThrow m) => ConduitT B.ByteString o m ()
fzfStdout =
  decodeUtf8C
    .| linesUnboundedC
    .| mapC parseFzfLine
    .| concatC
    .| mapM_C (liftIO . uncurry runAction)
