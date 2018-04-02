{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Lens
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson
import Data.Aeson.Lens
import Data.Default (def)
import Data.List (length, intersperse)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import System.IO (IOMode(..), withFile)
import Text.Pandoc (Pandoc(..))
import Text.Pandoc.Class (PandocMonad, runIO)
import Text.Pandoc.Definition
import Text.Pandoc.Lens
import Text.Pandoc.Readers
import Text.Pandoc.Writers (writeMarkdown)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.IO as T
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
  v <- thread "/tmp/optimized-header-sync.json"
  let msgs  = messages v
      outfile = "/tmp/out.md"
      mdocs = do docs <- mapM renderMessage msgs
                 let seps = concat $ intersperse [HorizontalRule] docs
                 writeMarkdown def (Pandoc nullMeta seps)
  edocs <- runIO mdocs
  case edocs of
    Left err   -> print err
    Right md -> do
      putStrLn ("writing " ++ outfile)
      T.writeFile outfile md


thread :: FilePath -> IO Value
thread file = do
  withFile file ReadMode $ \handle -> do
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

renderMessage :: (MonadIO m, PandocMonad m) => Message -> m [Block]
renderMessage Message{..} =
  let
    msg = "From: "<> msgFrom <> "\r\n" <>
          "To: " <> msgTo <> "\r\n\r\n" <>
          msgBody
    trunc n len = '[' : show (len - n) ++ " lines truncated]"
    rewriter bs =
      let
        len = length bs
        n   = 15
      in
        if len > n
          then take n bs ++ [ Para [ Str (trunc n len) ] ]
          else bs
  in
    do doc@(Pandoc meta_ blocks) <- readMarkdown def msg
       let bs = transformOf (plate . traverse . _BlockQuote) rewriter blocks
           newDoc = Pandoc meta_ bs
           native = A.encode doc
           nativeF = A.encode newDoc
       liftIO $ BS.writeFile "/tmp/native.json" native
       liftIO $ BS.writeFile "/tmp/native-filtered.json" nativeF
       return bs
