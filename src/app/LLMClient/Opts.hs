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
  RawOutput (..), Stream (..), System (..), Verbose (..), convertOptions,
  defaultHost, defaultModel )


{- HLINT ignore "Functor law" -}
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
  -- NOTE: The inner <$> is for Maybe, the outer <$> is for Parser
  <*> ( (System . pack <$>) <$> optional ( strOption
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
  <*> ( convertOptions <$> many ( strOption
        (  long "option"
        <> short 'o'
        <> metavar "KEY:VALUE"
        <> help "Options for the LLM, repeat for each option pair. See LLM OPTIONS below"
        )
      ))
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
  <*> ( Verbose <$> switch
        (  long "verbose"
        <> short 'v'
        <> help "Enable verbose output. Note: All logging goes to stderr"
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
    (  header (formatToString (t % " - Command line tool for interacting with an ollama server") (pack pn))
    <> footer'
    )


footer' :: InfoMod a
footer' = footerDoc . Just . pretty . format content . pack . showVersion $ version
  where content = [here|OVERVIEW

This software expects the prompt string on STDIN and will respond with the LLM
response on STDOUT

example

    $ echo "Why is the sky blue?" | llm-client -m 'some-fancy-model'

LLM OPTIONS

Some settings are communicated to the LLM as a map of options. Use them like this

    -o temperature:0.2

Some useful options

    option name   description                                                   default
    -------------------------------------------------------------------------------------
    seed          Sets the random number seed to use for generation. Setting    0
                  this to a specific number will make the model generate the
                  same text for the same prompt.
    temperature   The temperature of the model. Increasing the temperature      0.8
                  will make the model answer more creatively.
                  Range 0.0 to 1.0
    top_k         Reduces the probability of generating nonsense. A higher      40
                  value (e.g. 100) will give more diverse answers, while a
                  lower value (e.g. 10) will be more conservative.
                  Range 0 to 100
    top_p         Works together with top-k. A higher value (e.g., 0.95) will   0.9
                  lead to more diverse text, while a lower value (e.g., 0.5)
                  will generate more focused and conservative text.
                  Range unknown, guessing 0.0 to 1.0

For the complete list of LLM options, see

https://github.com/ollama/ollama/blob/main/docs/modelfile.md#valid-parameters-and-values

Version |] % t % "  Dino Morelli <dino@ui3.info>"
