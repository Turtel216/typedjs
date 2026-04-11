{-# LANGUAGE OverloadedStrings #-}

module Driver where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Desuger (lowerProgram)
import Diagnostic (renderDiagnostic)
import Emit (emitProgram)
import Parser
import Typecheck
import System.IO

compileSource :: FilePath -> T.Text -> Either T.Text T.Text
compileSource file src =
  case parseProgram file src of
    Left perr ->
      Left (T.pack perr)
    Right prog ->
      case inferProgram prog of
        Left terr ->
          Left (renderDiagnostic file src terr)
        Right _ ->
          Right (emitProgram (lowerProgram prog))
