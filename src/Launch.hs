module Launch (main) where

import Conduit
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Launch.Fzf as Fzf
import qualified Launch.InsertEmoji as InsertEmoji
import qualified Launch.LaunchApplication as LaunchApplication

main :: IO ()
main = runResourceT $ Fzf.run fzfStdin fzfStdout

toFzfStdinLine :: Fzf.Option -> Builder.Builder
toFzfStdinLine option =
  Builder.fromText (actionToText (Fzf.action option))
    <> "\FS"
    <> Fzf.payload option
    <> "\FS"
    <> Fzf.description option

actionToText :: Fzf.Action -> T.Text
actionToText Fzf.LaunchApplication = "launch"
actionToText Fzf.InsertEmoji = "emoji"

actionByText :: Map.Map T.Text Fzf.Action
actionByText =
  Map.fromList $
    fmap
      (\action -> (actionToText action, action))
      [minBound .. maxBound]

runAction :: Fzf.Action -> T.Text -> IO ()
runAction action payload = do
  case action of
    Fzf.LaunchApplication -> LaunchApplication.exec payload
    Fzf.InsertEmoji -> InsertEmoji.exec payload

parseFzfLine :: T.Text -> Maybe (Fzf.Action, T.Text)
parseFzfLine line =
  case T.splitOn "\FS" line of
    [actionText, payload', _] -> do
      action' <- Map.lookup actionText actionByText
      pure (action', payload')
    _ -> Nothing

fzfStdin :: MonadResource m => ConduitT i B.ByteString m ()
fzfStdin =
  fzfOptions
    .| mapC (TL.toStrict . Builder.toLazyText . toFzfStdinLine)
    .| unlinesC
    .| encodeUtf8C

fzfOptions :: MonadResource m => ConduitT i Fzf.Option m ()
fzfOptions = do
  LaunchApplication.options
  InsertEmoji.options

fzfStdout :: (MonadIO m, MonadThrow m) => ConduitT B.ByteString o m ()
fzfStdout =
  decodeUtf8C
    .| linesUnboundedC
    .| mapC parseFzfLine
    .| concatC
    .| mapM_C (liftIO . uncurry runAction)
