module Test.Main where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Prelude (Unit, discard, pure, show, ($))
import QuickServe (Capture(..), GET, JSON(..), POST, Query(..), RequestBody(..), notFound, ok, quickServe)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype Message = Message { message :: String }

derive newtype instance readMessage :: ReadForeign Message
derive newtype instance writeMessage :: WriteForeign Message

-- | This will serve three endpoints:
-- |
-- | - `/hello`, which returns the plain text string "Hello World!"
-- | - `/echo1`, which receives a JSON message in a POST body
-- | - `/echo2/<arg>`, which receives a plain text message to echo as a path argument
-- |
-- | Each of these can be tested with cURL:
-- |
-- | ```
-- | curl http://localhost:3000/hello
-- | curl http://localhost:3000/echo1 -XPOST -d '{"message": "test"}'
-- | curl http://localhost:3000/echo2/test
-- | ```
main :: Effect Unit
main = do
  let opts = { hostname: "localhost", port: 3000, backlog: Nothing }
  quickServe opts $
    let
      echo1 :: RequestBody (JSON Message)
            -> POST (JSON Message)
      echo1 (RequestBody (JSON (Message { message }))) = do
        liftEffect (log message)
        pure $ ok (JSON (Message { message }))

      echo2 :: Capture Int -> GET String
      echo2 (Capture message) = pure $ ok $ show message

      echo2' :: POST String
      echo2' = pure $ notFound

      hello :: Query { param :: Int } -> GET String
      hello (Query { param }) = pure $ ok "Hello, World!"
    in { echo1, echo2, echo2', hello }
