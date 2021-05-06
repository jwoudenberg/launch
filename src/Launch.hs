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

desktopFile :: Map.Map T.Text T.Text -> Maybe T.Text
desktopFile pairs = do
  name <- Map.lookup "Name" pairs
  exec <- Map.lookup "Exec" pairs
  pure (name <> "," <> exec)

desktopFileParser :: P.Parser (Map.Map T.Text T.Text)
desktopFileParser = do
  _ <- P.string "[Desktop Entry]"
  _ <- P.many1 P.endOfLine
  keyValuePairs mempty

keyValuePairs :: Map.Map T.Text T.Text -> P.Parser (Map.Map T.Text T.Text)
keyValuePairs pairs = do
  key <- P.takeWhile1 (/= '=')
  _ <- P.char '='
  val <- value ""
  _ <- P.many1 P.endOfLine
  P.option pairs (keyValuePairs (Map.insert key val pairs))

value :: T.Text -> P.Parser T.Text
value acc = do
  bit <- P.takeTill (\c -> P.isEndOfLine c || c == '%')
  let newAcc = acc <> bit
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
      pure (T.stripEnd newAcc)
    ]
