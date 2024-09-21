{-# LANGUAGE DuplicateRecordFields, OverloadedRecordDot #-}

module LLMClient.Common
  where

import Control.Arrow ( (&&&) )
import Data.Aeson ( Key, ToJSON, Value (Number, Object, String), object, toJSON )
import Data.Aeson.Key ( fromString )
import Data.Aeson.Types ( Pair, emptyObject )
import Data.String.Conv ( toS )
import Data.Text.Lazy qualified as TL
import GHC.Generics ( Generic )


data Model = Model TL.Text
  deriving Generic

instance ToJSON Model

defaultModel :: String
defaultModel = "llama3.1:8b"


newtype Prompt = Prompt TL.Text
  deriving Generic

instance ToJSON Prompt


newtype Stream = Stream Bool
  deriving Generic

instance ToJSON Stream


data OllamaRequest = OllamaRequest
  { model :: Model
  , prompt :: Prompt
  , stream :: Stream
  , options :: Maybe Value
  }
  deriving Generic

instance ToJSON OllamaRequest


mkLLMRequest :: Options -> TL.Text -> OllamaRequest
mkLLMRequest opts promptText = OllamaRequest opts.model (Prompt promptText) opts.stream (wrapMaybe opts.llmOptions.v)
  where
    wrapMaybe o@(Object _) = if o == emptyObject then Nothing else Just o
    wrapMaybe _ = Nothing


newtype Host = Host TL.Text

defaultHost :: String
defaultHost = "localhost:11434"

newtype System = System (Maybe TL.Text)

newtype RawOutput = RawOutput Bool

data Options = Options
  { host :: Host
  , system :: System
  , model :: Model
  , llmOptions :: LLMOptions
  , stream :: Stream
  , rawOutput :: RawOutput
  }


newtype LLMOptions = LLMOptions { v :: Value }
  deriving Generic

instance ToJSON LLMOptions

debugOptsJSON :: LLMOptions -> Value
debugOptsJSON = toJSON

convertOptions :: [String] -> LLMOptions
convertOptions rawStrings = LLMOptions . object . pairs' $ rawStrings

pairs' :: [String] -> [Pair]
pairs' = map (convertTypes . (takeWhile (/= ':') &&& (tail . dropWhile (/= ':'))))

convertTypes :: (String, String) -> (Key, Value)
convertTypes (keystr@"temperature", valstr) = (fromString keystr, Number . read $ valstr)
convertTypes (keystr@"presence_penalty", valstr) = (fromString keystr, Number . read $ valstr)
convertTypes (keystr, valstr) = (fromString keystr, String . toS $ valstr)
