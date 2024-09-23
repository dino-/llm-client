module LLMClient.HTTP
  where

import Data.Aeson ( Value (String), encode, toJSON )
import Data.Aeson.Lens ( key )
import Data.ByteString.Lazy qualified as BL
import Data.String.Conv ( toS )
import Data.Text.IO qualified as TS
import Lens.Micro ( (^.), (^?) )
import Network.Wreq ( post, responseBody )

import LLMClient.Common ( OllamaRequest, RawOutput (..) )
import LLMClient.System.Log ( debugM, lname )


doCompletion :: OllamaRequest -> IO BL.ByteString
doCompletion or' = do
  debugM lname . toS . encode $ or'
  let url = "http://localhost:11434/api/generate"
  response <- post url $ toJSON or'
  pure $ response ^. responseBody


display :: RawOutput -> BL.ByteString -> IO ()

display (RawOutput True) bs = BL.putStr bs

display (RawOutput False) bs = case bs ^? key "response" of
  (Just (String t)) -> TS.putStrLn t
  badResponse -> putStrLn $ "Something bad happened:\n" <> show badResponse
