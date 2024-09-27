module LLMClient.HTTP
  ( display
  , doCompletion
  )
  where

import Control.Exception ( tryJust )
import Data.Aeson ( Value (String), encode, toJSON )
import Data.Aeson.Lens ( key )
import Data.ByteString.Lazy qualified as BL
import Data.String.Conv ( toS )
import Data.Text.IO qualified as TS
import Formatting ( (%), (%+), formatToString, string, text )
import Lens.Micro ( (^.), (^?) )
import Network.HTTP.Client ( HttpException (HttpExceptionRequest,
  InvalidUrlException) )
import Network.Wreq ( post, responseBody )
import System.Exit ( exitFailure )

import LLMClient.Common ( Host (..), OllamaRequest, RawOutput (..) )
import LLMClient.System.Log ( debugM, emergencyM, errorM, lname )


doCompletion :: Host -> OllamaRequest -> IO BL.ByteString
doCompletion (Host host) or' = do
  debugM lname . toS . encode $ or'
  let url = formatToString ("http://" % text % "/api/generate") host
  eResponse <- tryJust acceptedExceptions . post url . toJSON $ or'
  case eResponse of
    Left (HttpExceptionRequest _ e) -> logAndExit $ show e
    Left (InvalidUrlException _ e) -> logAndExit $ show e
    Right response -> pure $ response ^. responseBody


acceptedExceptions :: Applicative f => HttpException -> f HttpException
acceptedExceptions = pure


logAndExit :: String -> IO a
logAndExit msg = do
  errorM lname $ formatToString ("Can't continue:" %+ string) msg
  exitFailure


display :: RawOutput -> BL.ByteString -> IO ()

display (RawOutput True) bs = BL.putStr bs

display (RawOutput False) bs = case bs ^? key "response" of
  (Just (String t)) -> TS.putStrLn t
  badResponse -> do
    emergencyM lname $
      "Something unexpected happened, LLM didn't create a response:\n" <> show badResponse
    exitFailure
