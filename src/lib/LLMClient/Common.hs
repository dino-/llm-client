{-# LANGUAGE DuplicateRecordFields, OverloadedRecordDot #-}

module LLMClient.Common
  where

import Control.Arrow ( (&&&) )
import Control.Monad ( when )
import Data.Aeson ( Key, ToJSON, Value (Number, Object, String),
  defaultOptions, genericToEncoding, genericToJSON, object, omitNothingFields,
  toEncoding, toJSON )
import Data.Aeson qualified as Aeson
import Data.Aeson.Key ( fromString )
import Data.Aeson.Types ( Pair, emptyObject )
import Data.Maybe ( catMaybes )
import Data.String.Conv ( toS )
import Data.Text.Lazy qualified as TL
import Formatting ( (%), formatToString, int, text )
import GHC.Generics ( Generic )
import Text.Read ( readMaybe )

import LLMClient.System.Log ( Priority (DEBUG) )


newtype Model = Model TL.Text
  deriving Generic

instance ToJSON Model

defaultModel :: String
defaultModel = "llama3.1:8b"


newtype Prompt = Prompt TL.Text
  deriving Generic

instance ToJSON Prompt


newtype Stream = Stream Bool
  deriving Generic

instance ToJSON Stream


data OllamaRequest = OllamaRequest
  { model :: Model
  , system :: Maybe System
  , prompt :: Prompt
  , stream :: Stream
  , options :: Maybe Value
  }
  deriving Generic

customOptions :: Aeson.Options
customOptions = defaultOptions { omitNothingFields = True }

instance ToJSON OllamaRequest where
  toJSON     = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions


mkLLMRequest :: Options -> TL.Text -> OllamaRequest
mkLLMRequest opts promptText = OllamaRequest opts.model opts.system
  (Prompt promptText) opts.stream (wrapMaybe opts.llmOptions.v)
  where
    wrapMaybe o@(Object _) = if o == emptyObject then Nothing else Just o
    wrapMaybe _ = Nothing


splitAtColon :: String -> Maybe (String, String)
splitAtColon combinedStr = do
  let (leftSide, rightSide) = takeWhile (/= ':') &&& dropWhile (/= ':') $ combinedStr
  when (null rightSide) Nothing
  pure (leftSide, tail rightSide)


data Host = Host TL.Text Int

instance Show Host where
  show (Host hostName port) = formatToString (text % ":" % int) hostName port


defaultHost :: Host
defaultHost = Host "localhost" 11434


hostFromString :: String -> Maybe Host
hostFromString combinedStr = do
  (hostName, portStr) <- splitAtColon combinedStr
  Host (toS hostName) <$> readMaybe portStr


newtype System = System TL.Text
  deriving Generic

instance ToJSON System

newtype RawOutput = RawOutput Bool

data Options = Options
  { host :: Host
  , system :: Maybe System
  , model :: Model
  , llmOptions :: LLMOptions
  , stream :: Stream
  , rawOutput :: RawOutput
  , verbose :: Verbose
  }


newtype LLMOptions = LLMOptions { v :: Value }
  deriving Generic

instance ToJSON LLMOptions

debugOptsJSON :: LLMOptions -> Value
debugOptsJSON = toJSON

convertOptions :: [String] -> LLMOptions
convertOptions = LLMOptions . object . pairs'

pairs' :: [String] -> [Pair]
pairs' = map convertTypes . catMaybes . map splitAtColon

convertTypes :: (String, String) -> (Key, Value)
convertTypes (keystr@"stop", valstr) = (fromString keystr, String . toS $ valstr)
convertTypes (keystr, valstr) = (fromString keystr, Number . read $ valstr)


newtype Verbose = Verbose Bool
  deriving Show


verbosityToPriority :: Verbose -> Maybe Priority
verbosityToPriority (Verbose True) = Just DEBUG
verbosityToPriority (Verbose False) = Nothing
