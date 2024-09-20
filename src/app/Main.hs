import Data.Text.Lazy.IO qualified as TL
import System.IO ( stdin )

import LLMClient.Common
import LLMClient.Http


main :: IO ()
main = do
  promptText <- TL.hGetContents stdin
  let or' = mkGenericRequest promptText
  doCompletion or' >>= display
