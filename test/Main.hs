{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.Golden
import System.FilePath ((</>), takeFileName)
import System.Directory (listDirectory, doesDirectoryExist)
import System.Process.Typed (proc, readProcess_)
import Control.Monad (filterM)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Driver

-- | Helper for the E2E test suite. Runs the compiler pipeline
compileToJS :: FilePath -> IO (Either T.Text T.Text)
compileToJS inputFile = do
  src <- T.readFile inputFile
  return $ compileSource inputFile src -- Assuming compileSource is imported

main :: IO ()
main = do
  -- Dynamically discover all test cases in the pass/ directory
  let passDir = "test/e2e/pass"
  passTests <- discoverTests passDir

  defaultMain $
    testGroup
      "Compiler E2E Tests"
      [ testGroup "Pass Cases" passTests
      ]

-- | Traverses the given directory and generates a test suite for each subfolder.
discoverTests :: FilePath -> IO [TestTree]
discoverTests baseDir = do
  contents <- listDirectory baseDir
  let paths = map (baseDir </>) contents
  -- Keep only directories (e.g., "001-basic-arithmetic")
  testDirs <- filterM doesDirectoryExist paths
  mapM mkTestCase testDirs

-- | Constructs the golden tests for a single test directory in pass/
mkTestCase :: FilePath -> IO TestTree
mkTestCase dir = do
  let testName        = takeFileName dir
      inputFile       = dir </> "input.tjs"
      expectedJsFile  = dir </> "expected.js"
      expectedOutFile = dir </> "expected.out"
      actualJsFile    = dir </> "actual.js"

  return $ testGroup testName
    [ 
      -- Test 1: Code Generation
      goldenVsString "1. Code Generation" expectedJsFile $ do
        result <- compileToJS inputFile
        case result of
          Left err -> 
            -- If a 'pass' test returns a compilation error, crash the test immediately.
            fail $ "Expected successful compilation, but got error:\n" ++ T.unpack err
          
          Right jsOutput -> do
            -- Save it to disk for Node to run, and for manual inspection
            T.writeFile actualJsFile jsOutput
            -- Convert Text -> Strict ByteString -> Lazy ByteString for tasty-golden
            return (BL.fromStrict $ TE.encodeUtf8 jsOutput)

    , 
      -- Test 2: Runtime Behavior
      goldenVsString "2. Runtime Behavior" expectedOutFile $ do
        result <- compileToJS inputFile
        case result of
          Left _ -> 
            -- No need to print the error again here, Test 1 will catch it.
            fail "Compilation failed, cannot execute."
          
          Right jsOutput -> do
            -- Ensure the file exists for this concurrent thread
            T.writeFile actualJsFile jsOutput
            
            -- Invoke Node.js, execute the file, and capture BOTH streams
            -- TODO: Chek if one of a list of runtimes(node, bun etc) and use that one
            let nodeProcess = proc "node" [actualJsFile]
            (stdout, stderr) <- readProcess_ nodeProcess 
            
            return (stdout <> stderr)
    ]
