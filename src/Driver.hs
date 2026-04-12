{-# LANGUAGE OverloadedStrings #-}

module Driver where

import qualified Data.Text as T
import Desuger (lowerProgram)
import Diagnostic (renderDiagnostic)
import Emit (emitProgram)
import Parser
import Typecheck

-- | Full Compilation pipeline
compileSource :: Bool -> FilePath -> T.Text -> Either T.Text T.Text
compileSource disableColor file src =
  case parseProgram file src of
    Left perr ->
      Left (T.pack perr)
    Right prog ->
      case inferProgram prog of
        Left terr ->
          Left (renderDiagnostic disableColor file src terr)
        Right _ ->
          Right (emitProgram (lowerProgram prog))
