{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Diagnostic rendering for type errors.
--
-- Produces colour-annotated error messages  with source-line snippets, caret underlines, and contextual
-- @note:@ / @help:@ messages.
module Diagnostic
  ( renderDiagnostic,
    prettyType,
  )
where

import Ast (Pos (..), Span (..))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Typecheck (IType (..), Note (..), TVarId, TypeError (..), TypeErrorKind (..))

ansi :: Text -> Text -> Text
ansi code t = "\ESC[" <> code <> "m" <> t <> "\ESC[0m"

red :: Text -> Text
red = ansi "1;31"

blue :: Text -> Text
blue = ansi "1;34"

bold :: Text -> Text
bold = ansi "1"

-- | Render an 'IType' as a user-friendly string.
prettyType :: IType -> Text
prettyType = \case
  TV n -> prettyTVar n
  TIntT -> "Int"
  TBoolT -> "Bool"
  TStringT -> "String"
  TNullT -> "Null"
  TFunT [] r -> "() -> " <> prettyType r
  TFunT [a] r -> prettyTypeAtom a <> " -> " <> prettyType r
  TFunT as r -> "(" <> T.intercalate ", " (map prettyType as) <> ") -> " <> prettyType r
  TArrayT t -> "[" <> prettyType t <> "]"
  TObjectT fs ->
    "{ " <> T.intercalate ", " [k <> ": " <> prettyType v | (k, v) <- M.toList fs] <> " }"
  TCon n [] -> n
  TCon n ts -> n <> "<" <> T.intercalate ", " (map prettyType ts) <> ">"

-- | Parenthesise function types when they appear as arguments to another
-- function type.
prettyTypeAtom :: IType -> Text
prettyTypeAtom t@(TFunT {}) = "(" <> prettyType t <> ")"
prettyTypeAtom t = prettyType t

-- | Render a type variable id as a lowercase letter (a, b, …, z, a1, b1, …).
prettyTVar :: TVarId -> Text
prettyTVar n
  | n < 26 = T.singleton (toEnum (fromEnum 'a' + n))
  | otherwise =
      T.singleton (toEnum (fromEnum 'a' + (n `mod` 26)))
        <> tshow (n `div` 26)

-- | Stable error code for each error kind.
errorCode :: TypeErrorKind -> Text
errorCode = \case
  TypeMismatch {} -> "E0001"
  InfiniteType {} -> "E0002"
  UnboundVariable {} -> "E0003"
  MissingField {} -> "E0004"
  DuplicateBinding {} -> "E0005"
  ImmutableAssign {} -> "E0006"
  UnboundType {} -> "E0007"
  DuplicateType {} -> "E0008"
  TypeArityMismatch {} -> "E0009"
  OtherError {} -> "E0010"

-- | Short human-readable title for the error kind.
errorTitle :: TypeErrorKind -> Text
errorTitle = \case
  TypeMismatch {} -> "type mismatch"
  InfiniteType {} -> "infinite type"
  UnboundVariable {} -> "cannot find value in this scope"
  MissingField {} -> "missing field"
  DuplicateBinding {} -> "duplicate definition"
  ImmutableAssign {} -> "cannot assign to immutable variable"
  UnboundType {} -> "undefined type"
  DuplicateType {} -> "duplicate type definition"
  TypeArityMismatch {} -> "wrong number of type arguments"
  OtherError {} -> "type error"

-- | Detailed message shown under the source underline.
errorDetail :: TypeErrorKind -> Text
errorDetail = \case
  TypeMismatch t1 t2 ->
    "expected `" <> prettyType t2 <> "`, found `" <> prettyType t1 <> "`"
  InfiniteType v t ->
    "type variable `" <> prettyTVar v <> "` occurs in `" <> prettyType t <> "`"
  UnboundVariable _name ->
    "not found in this scope"
  MissingField name ->
    "no field `" <> name <> "` on this type"
  DuplicateBinding name ->
    "`" <> name <> "` is already defined in this scope"
  ImmutableAssign name ->
    "cannot assign to `" <> name <> "`"
  UnboundType name ->
    "type `" <> name <> "` is not defined"
  DuplicateType name ->
    "type `" <> name <> "` is already defined"
  TypeArityMismatch name expected got ->
    "`"
      <> name
      <> "` expects "
      <> tshow expected
      <> " type argument(s) but "
      <> tshow got
      <> " were given"
  OtherError msg -> msg

-- | Render the coloured header line:  @error[E0001]: type mismatch@
errorHeader :: TypeErrorKind -> Text
errorHeader kind =
  red ("error[" <> errorCode kind <> "]") <> bold (": " <> errorTitle kind)

-- | Render a source-line snippet with caret underline and message.
--
-- @
-- |
-- 6 | let b = id(true);
-- |         ^^^^^^^^ expected `Int`, found `Bool`
-- |
-- @
renderSnippet :: Int -> Text -> Span -> Text -> Text
renderSnippet gw src (Span (Pos line col) (Pos endLine endCol)) msg =
  let srcLines = T.lines src
      lineStr = tshow line
      lineNum = T.justifyRight gw ' ' lineStr
      pad = T.replicate (gw + 1) " "
      emptyG = pad <> blue "|"
      srcG = blue (lineNum <> " |") <> " "
      ulG = pad <> blue "|" <> " "
   in if line >= 1 && line <= length srcLines
        then
          let srcLine = srcLines !! (line - 1)
              lineLen = T.length srcLine
              startOff = max 0 (col - 1)
              caretLen
                | line == endLine && endCol > col = endCol - col
                | otherwise = max 1 (lineLen - startOff)
              safeLen = max 1 (min caretLen (lineLen - startOff + 1))
              spacing = T.replicate startOff " "
           in emptyG
                <> "\n"
                <> srcG
                <> srcLine
                <> "\n"
                <> ulG
                <> spacing
                <> red (T.replicate safeLen "^")
                <> " "
                <> red msg
                <> "\n"
                <> emptyG
                <> "\n"
        else -- Span outside source (e.g. dummy span)
          emptyG
            <> "\n"
            <> ulG
            <> red msg
            <> "\n"
            <> emptyG
            <> "\n"

renderNote :: Int -> Note -> Text
renderNote gw note =
  let pad = T.replicate (gw + 1) " "
   in case note of
        NoteText txt -> pad <> blue "= " <> bold "note: " <> txt <> "\n"
        NoteHelp txt -> pad <> blue "= " <> bold "help: " <> txt <> "\n"
        NoteSpan _ txt -> pad <> blue "= " <> bold "note: " <> txt <> "\n"

-- | Render a complete, coloured diagnostic string for a 'TypeError'.
--
-- @
-- error[E0001]: type mismatch
-- --> example.tjs:6:9
-- |
-- 6 | let b = id(true);
-- |         ^^^^^^^^ expected `Int`, found `Bool`
-- |
-- = note: …
-- @
renderDiagnostic :: FilePath -> Text -> TypeError -> Text
renderDiagnostic fp src (TypeError mSpan kind notes) =
  header
    <> "\n"
    <> locationStr
    <> snippetStr
    <> notesStr
  where
    gutterW = case mSpan of
      Just (Span (Pos l _) _) -> max 1 (length (show l))
      Nothing -> 1

    header = errorHeader kind

    locationStr = case mSpan of
      Nothing -> ""
      Just (Span (Pos l c) _) ->
        T.replicate gutterW " "
          <> blue "--> "
          <> T.pack fp
          <> ":"
          <> tshow l
          <> ":"
          <> tshow c
          <> "\n"

    snippetStr = case mSpan of
      Nothing ->
        let pad = T.replicate (gutterW + 1) " "
         in pad
              <> blue "|"
              <> "\n"
              <> pad
              <> blue "| "
              <> red (errorDetail kind)
              <> "\n"
              <> pad
              <> blue "|"
              <> "\n"
      Just sp -> renderSnippet gutterW src sp (errorDetail kind)

    notesStr = T.concat [renderNote gutterW n | n <- notes]

tshow :: (Show a) => a -> Text
tshow = T.pack . show
