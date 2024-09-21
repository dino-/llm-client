module LLMClient.Http
  where

import Data.Aeson ( Value (String), toJSON )
import Data.Aeson.Lens ( key )
import Data.ByteString.Lazy qualified as BL
import Data.Text.IO qualified as TS
import Lens.Micro ( (^?) )
import Network.Wreq ( post, responseBody )

import LLMClient.Common ( OllamaRequest, RawOutput (..) )


doCompletion :: OllamaRequest -> IO (Maybe BL.ByteString)
doCompletion or' = do
  let url = "http://localhost:11434/api/generate"
  response <- post url $ toJSON or'
  pure $ response ^? responseBody


display :: RawOutput -> Maybe BL.ByteString -> IO ()

display (RawOutput True) (Just bs) = BL.putStr bs

display (RawOutput False) (Just bs) = case bs ^? key "response" of
  (Just (String t)) -> TS.putStrLn t
  badResponse -> putStrLn $ "Something bad happened:\n" <> show badResponse

display _ Nothing = putStrLn "Not sure what happened, result of response ^? responseBody was Nothing"


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
