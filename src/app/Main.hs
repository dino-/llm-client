{-# LANGUAGE OverloadedRecordDot #-}

import Data.Text.Lazy.IO qualified as TL
import System.IO ( stdin )

import LLMClient.Common ( mkLLMRequest )
import LLMClient.Http ( display, doCompletion )
import LLMClient.Opts ( parseOpts )


main :: IO ()
main = do
  opts <- parseOpts
  promptText <- TL.hGetContents stdin
  let or' = mkLLMRequest opts promptText
  doCompletion or' >>= display
