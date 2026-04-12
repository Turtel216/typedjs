-- | Command line interface definition
module Cli where

import Options.Applicative

-- | CLI Options
data Options = Options
  { sourceFile :: [FilePath] -- ^ Source file
  , optimizations :: Bool     -- ^ Enable optimizations
  , outputFile :: Maybe FilePath    -- ^ Output file emited by compiler      
  , disableColor :: Bool      -- ^ Disable colored output
  } deriving (Show)

-- | Parser for CLI options
optionsParser :: Parser Options
optionsParser = Options
  <$> some (argument str
        ( metavar "SOURCE_FILES..."
       <> help "Source file to process" ))
  <*> switch
        ( long "opt"
       <> short 'O'
       <> help "Enable compiler optimizations" )
  <*> optional (option str 
        ( long "output"
       <> short 'o'
       <> help "Output file emited by Compiler[default out.js]" ))
  <*> fmap (== Just "never") (optional (option str
        ( long "color"
       <> help "Disable coloured output (--color=never)" )))

-- | Full parser with help text
optsInfo :: ParserInfo Options
optsInfo = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "Compile TypeJs files to readable javascript"
 <> header "TypeJs - type-safe Javascript dialect" )
