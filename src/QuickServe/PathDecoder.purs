module QuickServe.PathDecoder where

import Data.Int as Int
import Data.Maybe (Maybe(..))

class PathDecoder a where
  decodePath :: String -> Maybe a

instance pathDecoderInt :: PathDecoder Int where
  decodePath = Int.fromString

instance pathDecoderString :: PathDecoder String where
  decodePath = Just
