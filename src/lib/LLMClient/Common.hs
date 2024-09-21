{-# LANGUAGE DuplicateRecordFields, OverloadedRecordDot #-}

module LLMClient.Common
  where

import Data.Aeson ( ToJSON )
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
  }
  deriving Generic

instance ToJSON OllamaRequest


mkLLMRequest :: Options -> TL.Text -> OllamaRequest
mkLLMRequest opts promptText = OllamaRequest opts.model (Prompt promptText) opts.stream


newtype Host = Host TL.Text

defaultHost :: String
defaultHost = "localhost:11434"

newtype System = System (Maybe TL.Text)

data Options = Options
  { host :: Host
  , system :: System
  , model :: Model
  , stream :: Stream
  }
