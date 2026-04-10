{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cli
import qualified Data.Text.IO as T
import Desuger (lowerProgram)
import Diagnostic (renderDiagnostic)
import Emit (emitProgram)
import Options.Applicative
import Parser
import Typecheck

-- TODO: Refactor this mess
main :: IO ()
main = do
  opts <- execParser optsInfo
  let file = head $ sourceFile opts
      outputF = case (outputFile opts) of
        Just ofile -> ofile
        Nothing -> "out.js"
  src <- T.readFile file
  case parseProgram file src of
    Left perr -> putStrLn perr
    Right prog ->
      case inferProgram prog of
        Left terr -> T.putStrLn (renderDiagnostic file src terr)
        Right _ -> do
          let js = emitProgram (lowerProgram prog)
          T.writeFile outputF js
          putStrLn $ "Generated " ++ outputF
