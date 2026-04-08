{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as T
import Parser
import Typecheck
import Desuger (lowerProgram)
import Emit (emitProgram)
import qualified Data.Text.IO as T
import Options.Applicative
import Cli

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
            Left terr -> putStrLn ("Type error: " <> show terr)
            Right _ -> do
                let js = emitProgram (lowerProgram prog)
                T.writeFile outputF js
                putStrLn $ "Generated " ++ outputF
