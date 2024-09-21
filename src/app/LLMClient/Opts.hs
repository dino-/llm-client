{-# LANGUAGE DuplicateRecordFields, OverloadedRecordDot, QuasiQuotes #-}

module LLMClient.Opts
  ( parseOpts
  )
  where

import Data.Text.Lazy ( pack )
import Data.Version ( showVersion )
import Formatting ( (%), format, formatToString )
import Formatting.ShortFormatters ( t )
import Options.Applicative
import Paths_llm_client ( version )
import Prettyprinter ( pretty )
import System.Environment ( getProgName )
import Text.Heredoc ( here )

import LLMClient.Common ( Host (..), Model (..), Options (Options),
  RawOutput (..), Stream (..), System (..), defaultHost, defaultModel )


parser :: Parser Options
parser = Options
  <$> ( Host . pack <$> strOption
        (  long "host"
        <> short 'H'
        <> metavar "HOST:PORT"
        <> help "Host and port where ollama serve is running"
        <> showDefault
        <> value defaultHost
        )
      )
  <*> ( System . maybe Nothing (Just . pack) <$> optional (option auto
        (  long "system"
        <> short 's'
        <> metavar "STR"
        <> help "System parameter"
        )
      ))
  <*> ( Model . pack <$> strOption
        (  long "model"
        <> short 'm'
        <> metavar "MODEL_ID"
        <> help "Model identifier, see available models with `ollama list`"
        <> showDefault
        <> value defaultModel
        )
      )
  <*> ( Stream <$> switch
        (  long "stream"
        <> help "Response will be returned as a stream of objects"
        )
      )
  <*> ( RawOutput <$> switch
        (  long "raw-output"
        <> short 'r'
        <> help "Output the entire JSON response from the LLM"
        )
      )


versionHelper :: String -> Parser (a -> a)
versionHelper progName =
  infoOption (formatToString (t % " " % t) (pack progName) (pack . showVersion $ version)) $ mconcat
  [ long "version"
  , help "Show version information"
  , hidden
  ]


parseOpts :: IO Options
parseOpts = do
  pn <- getProgName
  execParser $ info (parser <**> helper <**> versionHelper pn)
    (  header (formatToString (t % " - Calculations using gathered Glenwood South noise complaints") (pack pn))
    <> footer'
    )


footer' :: InfoMod a
footer' = footerDoc . Just . pretty . format content . pack . showVersion $ version
  where content = [here|OVERVIEW

llm-client is a tool for...

Version |] % t % "  Dino Morelli <dino@ui3.info>"
