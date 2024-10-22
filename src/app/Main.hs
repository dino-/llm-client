{-# LANGUAGE OverloadedRecordDot #-}

import Data.Text.Lazy.IO qualified as TL
import System.IO ( stdin )

import LLMClient.Common ( Options (host, rawOutput, verbose), mkLLMRequest,
  verbosityToPriority )
import LLMClient.HTTP ( display, doCompletion )
import LLMClient.Opts ( parseOpts )
import LLMClient.System.Log ( infoM, initLogging, lname )


main :: IO ()
main = do
  opts <- parseOpts
  initLogging . verbosityToPriority . verbose $ opts
  infoM lname "Logging configured"
  infoM lname "Args parsed"

  promptText <- TL.hGetContents stdin
  let or' = mkLLMRequest opts promptText
  doCompletion opts.host or' >>= display opts.rawOutput
