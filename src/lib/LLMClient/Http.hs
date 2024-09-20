module LLMClient.Http
  where

import Data.Aeson ( Value (String), toJSON )
import Data.Aeson.Lens  -- FIXME
-- import Data.ByteString.Lazy qualified as BL
import Data.Text.IO qualified as TS
import Lens.Micro  -- FIXME
import Network.Wreq  -- FIXME

import LLMClient.Common


doCompletion :: OllamaRequest -> IO (Maybe Value)
doCompletion or' = do
  let url = "http://localhost:11434/api/generate"
  response <- post url $ toJSON or'
  pure $ response ^? responseBody . key "response"


display :: Maybe Value -> IO ()
display response = case response of
  (Just (String t)) -> TS.putStrLn t
  badResponse -> putStrLn $ "Something bad happened:\n" <> show badResponse


-- import Control.Monad.Except ( MonadError, throwError )
-- import Control.Monad.Trans ( MonadIO, liftIO )
-- import Data.ByteString.Lazy qualified as BL
-- import Data.String.Conv ( toS )
-- import Data.Text ( Text )
-- import Network.Curl ( CurlCode (CurlOK), CurlOption (CurlHttpHeaders,
--   CurlVerbose), curlGetString )
-- import System.FilePath ( (</>) )

-- import NNW.Data.Common ( Limit, limitToRowStr )


-- getSheetData :: (MonadIO m, MonadError String m) => BL.ByteString -> Limit -> m Text
-- getSheetData signedJWT limit = do
--   let url = "http://localhost:11434/api/generate"
--   let opts =
--         [ CurlVerbose False  -- Set to True for debugging
--         ]
--   curlResult <- liftIO $ curlGetString url opts
--   resultToExcept curlResult


-- resultToExcept :: (MonadIO m, MonadError String m) => (CurlCode, String) -> m Text
-- resultToExcept (CurlOK, responseBody) = pure . toS $ responseBody
-- resultToExcept (curlCode, _) = throwError . show $ curlCode
