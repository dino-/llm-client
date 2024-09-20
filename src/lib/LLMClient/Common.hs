module LLMClient.Common
  where

import Data.Text.Lazy qualified as TL


data Model = Llama3_1_8b

instance Show Model where
  show Llama3_1_8b = "llama3.1:8b"


newtype Prompt = Prompt TL.Text


newtype Stream = Stream Bool


data OllamaRequest = OllamaRequest
  { model :: Model
  , prompt :: Prompt
  , stream :: Stream
  }
