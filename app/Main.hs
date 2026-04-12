{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cli
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Desuger (lowerProgram)
import Diagnostic (renderDiagnostic)
import Emit (emitProgram)
import Options.Applicative
import Parser
import Typecheck
import System.IO
import Driver

main :: IO ()
main = do
  opts <- execParser optsInfo
  let file = head $ sourceFile opts
      outputF = case outputFile opts of
        Just ofile -> ofile
        Nothing -> "out.js"

  src <- TIO.readFile file

  case compileSource file src of
    Left err -> do
      -- Print compilation errors to stderr so they don't pollute stdout
      TIO.hPutStrLn stderr err
    Right js -> do
      TIO.writeFile outputF js
      putStrLn $ "Generated " ++ outputF
