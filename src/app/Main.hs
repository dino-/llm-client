import LLMClient.Common
import LLMClient.Http


main :: IO ()
main = do
  let or' = mkGenericRequest "Why is the sky blue?"
  doCompletion or' >>= display
