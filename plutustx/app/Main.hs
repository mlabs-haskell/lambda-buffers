-- \| A short example demonstrating how one can use the types and instances
-- generated by the PlutusTx backend.

import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import LambdaBuffers.Example.PlutusTx (Content (Content'Emoji, Content'Text), Emoji (Emoji'ThumbsDown, Emoji'ThumbsUp))
import LambdaBuffers.Plutus.V1 (Bytes)
import PlutusLedgerApi.V1.Bytes (LedgerBytes (LedgerBytes))
import PlutusTx.Builtins (toBuiltin)
import PlutusTx.Eq qualified

someText :: Bytes
someText = LedgerBytes $ toBuiltin $ encodeUtf8 $ Text.pack "Some text"

main :: IO ()
main = do
  print $ Emoji'ThumbsUp PlutusTx.Eq.== Emoji'ThumbsDown

  print $ Emoji'ThumbsUp PlutusTx.Eq.== Emoji'ThumbsUp

  print $ Content'Text someText PlutusTx.Eq.== Content'Emoji Emoji'ThumbsUp
