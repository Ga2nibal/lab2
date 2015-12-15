module Modules.Arguments where

import Options.Applicative
import Modules.Helpers
-------------------------Options data---------------------     

data Options = Options
  { input          :: String
  , output         :: PrintSource
  , retryCount     :: Int
  , percent        :: Int
  , devider        :: String
  , skipFirstLine  :: Bool
  , skipFirstCol   :: Bool
  , skipLastCol    :: Bool}

-------------------------Helpers---------------------     

strOptionWithDefault :: String -> Mod OptionFields String -> Parser String
strOptionWithDefault = optionWithDefault (strOption)

optionWithDefault :: (Mod OptionFields a -> Parser a) -> a -> Mod OptionFields a -> Parser a
optionWithDefault parser defVal flags = parser flags <|> pure defVal

-------------------------Parser---------------------     

options :: Parser Options
options = Options
     <$> inputOption
     <*> outputFileOption
     <*> retryCountOption
     <*> percentOption
     <*> deviderOption
     <*> skipFLOption
     <*> skipFCOption  
     <*> skipLCOption

-------------------------Options---------------------   

inputOption = argument str (metavar "FILE")

retryCountOption :: Parser Int
retryCountOption = option auto
            ( long "retry-count"
           <> short 'r'
           <> metavar "R"
           <> help "Retry count" )

percentOption :: Parser Int
percentOption = option auto
            ( long "study-percent"
           <> short 'p'
           <> metavar "P"
           <> help "Percent of study part 1..100" )


outputFileOption = optional . strOption $
         ( long "output"
        <> metavar "FILE"
        <> help "File to output result. Default output is console." )

deviderOption = strOptionWithDefault defaultDevider
         (long "devider"
        <> metavar "DEVIDER"
        <> help helpText)
            where defaultDevider = ","
                  helpText = "Elements devider. Default is '" ++ defaultDevider ++ "'"

skipFLOption = switch
         ( long "skip-first-line"
        <> help "Skip first line in file" )

skipFCOption = switch
         ( long "skip-first-column"
        <> help "Skip first columt in file" ) 

skipLCOption = switch
         ( long "skip-last-column"
        <> help "Skip last columt in file" )

-------------------------Default info---------------------     

opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Naive Bayes classificator programm. Output is most accurate classificator params"
     <> header "Naive Bayes classificator" )