module LLMClient.System.Log
  ( initLogging, lname
  , logTest

  -- Re-exported from System.Log.Logger
  , Priority (..), debugM, infoM, noticeM, warningM, errorM
  , criticalM, alertM, emergencyM
  )
  where

import Data.Maybe ( fromMaybe )
import System.IO ( stderr )
import System.Log.Formatter ( simpleLogFormatter )
import System.Log.Handler ( setFormatter )
import System.Log.Handler.Simple ( streamHandler )
import System.Log.Logger


lname :: String
lname = "normal-output"


initLogging :: Maybe Priority -> IO ()
initLogging mLogPriority = do
  let logPriority = fromMaybe NOTICE mLogPriority

  -- Remove the root logger's default handler that writes every
  -- message to stderr!
  updateGlobalLogger rootLoggerName removeHandler

  errH <- flip setFormatter (simpleLogFormatter "[$time : $prio] $msg")
    <$> streamHandler stderr DEBUG
  updateGlobalLogger lname $ setHandlers [errH]
  updateGlobalLogger lname $ setLevel logPriority


-- Test function to generate every kind of log message
logTest :: IO ()
logTest = do
  debugM     lname "log test message DEBUG 1 of 8"
  infoM      lname "log test message INFO 2 of 8"
  noticeM    lname "log test message NOTICE 3 of 8"
  warningM   lname "log test message WARNING 4 of 8"
  errorM     lname "log test message ERROR 5 of 8"
  criticalM  lname "log test message CRITICAL 6 of 8"
  alertM     lname "log test message ALERT 7 of 8"
  emergencyM lname "log test message EMERGENCY 8 of 8"
