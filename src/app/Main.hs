{-# LANGUAGE OverloadedRecordDot #-}

import Data.Text.Lazy.IO qualified as TL
import System.IO ( stdin )

import LLMClient.Common ( Options (rawOutput), mkLLMRequest )
import LLMClient.HTTP ( display, doCompletion )
import LLMClient.Opts ( parseOpts )
import LLMClient.System.Log ( infoM, initLogging, lname )
-- import LLMClient.System.Log ( Priority (DEBUG) )


main :: IO ()
main = do
  opts <- parseOpts
  -- initLogging $ Just DEBUG
  initLogging Nothing
  infoM lname "Logging configured"
  infoM lname "Args parsed"

  promptText <- TL.hGetContents stdin
  let or' = mkLLMRequest opts promptText
  doCompletion or' >>= display opts.rawOutput
