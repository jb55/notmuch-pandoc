{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Lens
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.Aeson.Lens
import Data.Default (def)
import Data.List (length, intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import System.IO (Handle, stdin, stderr, hPutStrLn)
import Text.Pandoc (Pandoc(..))
import Data.Text.ICU.Replace (replaceAll)
import Text.Pandoc.Class (PandocMonad, runIO)
import Text.Pandoc.Definition
import Text.Pandoc.Lens
import Text.Pandoc.Options
import Text.Pandoc.Readers
import Text.Pandoc.Writers (writeEPUB3)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.HashMap.Lazy as Map


data Message =
  Message {
    msgFrom :: Text
  , msgTo   :: Text
  , msgBody :: Text
  }
  deriving Show

main :: IO ()
main = do
  v <- thread stdin
  let msgs  = messages v
      mdocs = do docs <- mapM renderMessage msgs
                 let seps = intercalate [HorizontalRule] docs
                 writeEPUB3 def (Pandoc nullMeta seps)
  edocs <- runIO mdocs
  case edocs of
    Left err   -> print err
    Right md -> do
      hPutStrLn stderr "writing out.epub"
      BS.writeFile "out.epub" md


thread :: Handle -> IO Value
thread handle = do
  txt <- BS.hGetContents handle
  maybe (fail "could not decode") return (decode txt)


hasKeyWith :: Text -> (Value -> Bool) -> Value -> Bool
hasKeyWith k fn (Object o) = maybe False fn (Map.lookup k o)
hasKeyWith _ _ _  = False


hasKey :: Text -> Value -> Bool
hasKey k = hasKeyWith k (const True)


messages :: Value -> [Message]
messages msgs =
  msgs ^.. cosmos . filtered (hasKey "body") . to parseMessage


parseMessage :: Value -> Message
parseMessage msg =
  let
    from_ = msg ^? key "headers" . key "From" . _String
    to_   = msg ^? key "headers" . key "To" . _String
    body_ = msg ^? cosmos . filtered (hasKeyWith "content-type" (== "text/plain"))
                          . key "content"
                          . _String
  in
    Message {
      msgFrom = fromMaybe "" from_
    , msgTo   = fromMaybe "" to_
    , msgBody = fromMaybe "" body_
    }

fixup :: Text -> Text
fixup = replaceAll "^\\s*>" "    "

renderMessage :: (MonadIO m, PandocMonad m) => Message -> m [Block]
renderMessage Message{..} =
  let
    opts = def { readerExtensions = getDefaultExtensions "markdown_github" }
    nl = "\r\n\r\n"
    msg = "**From** "<> msgFrom <> nl <>
          "**To** " <> msgTo <> nl <>
          fixup msgBody <> nl
  in
    do doc@(Pandoc meta_ blocks) <- readMarkdown opts msg
       return blocks
