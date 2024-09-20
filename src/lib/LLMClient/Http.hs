module LLMClient.Http
  where

import Data.Aeson ( toJSON )
import Network.Wreq  -- FIXME


-- doCompletion :: OllamaRequest -> IO (CurlCode, String)
doCompletion or = do
  let url = "http://localhost:11434/api/generate"
  post url $ toJSON or


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
