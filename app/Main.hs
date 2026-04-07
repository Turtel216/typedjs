{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as T
import Parser
import Typecheck

main :: IO ()
main = do
  src <- T.readFile "example.tjs"
  case parseProgram "example.tjs" src of
    Left perr -> putStrLn perr
    Right prog ->
      case inferProgram prog of
        Left terr -> putStrLn ("Type error: " <> show terr)
        Right env -> putStrLn ("Typecheck ok. Top-level env:\n" <> show env)
