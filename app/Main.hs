{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as T
import Parser
import Typecheck
import Desuger (lowerProgram)
import Emit (emitProgram)
import qualified Data.Text.IO as T

main :: IO ()
main = do
  src <- T.readFile "example.tjs"
  case parseProgram "example.tjs" src of
    Left perr -> putStrLn perr
    Right prog ->
        case inferProgram prog of
            Left terr -> putStrLn ("Type error: " <> show terr)
            Right _ -> do
                let js = emitProgram (lowerProgram prog)
                T.writeFile "out.js" js
                putStrLn "Generated out.js"
