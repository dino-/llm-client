module LLMClient.Common
  where

import Data.Aeson ( ToJSON )
import Data.Text.Lazy qualified as TL
import GHC.Generics ( Generic )


data Model = Model TL.Text
  deriving Generic

instance ToJSON Model


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


mkGenericRequest :: TL.Text -> OllamaRequest
mkGenericRequest promptText = OllamaRequest (Model "llama3.1:8b") (Prompt promptText) (Stream False)
