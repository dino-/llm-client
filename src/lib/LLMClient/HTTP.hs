{-# LANGUAGE DataKinds, TypeOperators #-}

module LLMClient.HTTP
  ( display
  , doCompletion
  )
  where

import Data.Aeson ( Value (Object, String), encode )
import Data.Aeson.KeyMap qualified as A
import Data.ByteString.Lazy qualified as BL
import Data.Proxy ( Proxy (..) )
import Data.String.Conv ( toS )
import Data.Text.Lazy qualified as TL
import Data.Text.IO qualified as TS
import Formatting ( (%+), formatToString, string )
import Network.HTTP.Client ( defaultManagerSettings, newManager )
import Servant.API ( (:>), JSON, Post, ReqBody )
import Servant.Client ( BaseUrl (..), Scheme (Http), ClientM, client,
  mkClientEnv, runClientM )
import System.Exit ( exitFailure )

import LLMClient.Common ( Host (..), OllamaRequest, RawOutput (..) )
import LLMClient.System.Log ( debugM, emergencyM, errorM, lname )


{- NOTE: The reason we've chosen Aeson's Value type for the response is we want
   to do one of two things with these responses:

   1. Display only the LLM response text (in the JSON Value's 'response' field)
   2. Dump the entire JSON response as-is (the -r|--raw-output behavior)

   We don't need to model the entire Ollama response JSON document to achieve these goals.
-}
type API = "api" :> "generate" :> ReqBody '[JSON] OllamaRequest :> Post '[JSON] Value

-- A Proxy to our API
api :: Proxy API
api = Proxy


completion
  :: OllamaRequest  -- ^ Value for the request body
  -> ClientM Value
completion = client api


doCompletion :: Host -> OllamaRequest -> IO Value
doCompletion host or' = do
  debugM lname . toS . encode $ or'
  manager' <- newManager defaultManagerSettings
  let (hostName, port) = splitHost host
  eres <- runClientM (completion or')
    (mkClientEnv manager' (BaseUrl Http hostName port ""))
  case eres of
    Left err -> logAndExit $ show err
    Right v@(Object _) -> pure v
    Right somethingElse -> logAndExit $ show somethingElse


splitHost :: Host -> (String, Int)
splitHost (Host t) = (toS hostName, readInt portStr)
  where
    (hostName, portStr) = TL.breakOn ":" t
    readInt = read . toS . TL.tail


logAndExit :: String -> IO a
logAndExit msg = do
  errorM lname $ formatToString ("Can't continue:" %+ string) msg
  exitFailure


display :: RawOutput -> Value -> IO ()

display (RawOutput True) v = BL.putStr $ encode v <> "\n"

display (RawOutput False) (Object keyMap) = case A.lookup "response" keyMap of
  Just (String t) -> TS.putStrLn t
  Just somethingElse -> emergencyM lname $ show somethingElse
  Nothing -> emergencyM lname $ show keyMap

display _ somethingElse = emergencyM lname $ show somethingElse
