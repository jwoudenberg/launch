module Launch (main) where

import Conduit
import qualified Data.Attoparsec.Text as P
import qualified Data.Conduit.Combinators as C
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath

-- $> main

main :: IO ()
main = do
  xdgDataDirs <- Environment.getEnv "XDG_DATA_DIRS"
  runResourceT $
    runConduit $
      yield xdgDataDirs
        .| C.splitOnUnboundedE (== ':')
        .| mapC (FilePath.</> "applications")
        .| filterMC (liftIO . Directory.doesDirectoryExist)
        .| awaitForever sourceDirectory
        .| filterC (FilePath.isExtensionOf ".desktop")
        .| awaitForever sourceFile
        .| C.concatMap TE.decodeUtf8'
        .| mapC (P.parseOnly desktopFileParser)
        .| C.concat
        .| mapC desktopFile
        .| C.concat
        .| printC

desktopFile :: Map.Map T.Text T.Text -> Maybe (T.Text, T.Text)
desktopFile pairs = do
  name <- Map.lookup "Name" pairs
  exec <- Map.lookup "Exec" pairs
  pure (name, exec)

desktopFileParser :: P.Parser (Map.Map T.Text T.Text)
desktopFileParser = do
  _ <- P.string "[Desktop Entry]"
  _ <- P.many1 P.endOfLine
  keyValuePairs mempty

keyValuePairs :: Map.Map T.Text T.Text -> P.Parser (Map.Map T.Text T.Text)
keyValuePairs pairs = do
  key <- P.takeWhile1 (/= '=')
  _ <- P.char '='
  val <- P.takeWhile (not . P.isEndOfLine)
  _ <- P.many1 P.endOfLine
  P.option pairs (keyValuePairs (Map.insert key val pairs))
