{-# LANGUAGE OverloadedStrings #-}

-- | A golden value testsuite for fix-whitespace.
--

module Main where

import           Data.ByteString.Lazy    ( ByteString )
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText

import           System.FilePath         ( takeBaseName, replaceExtension )

import           Test.Tasty              ( defaultMain, TestTree, testGroup )
import           Test.Tasty.Golden       ( goldenVsString, findByExtension )

import           Data.Text.FixWhitespace ( CheckResult(CheckOK, CheckViolation, CheckIOError)
                                         , checkFile, displayLineError, defaultTabSize )

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  files <- findByExtension [".txt"] "test"
  return $ testGroup "Golden tests"
    [ goldenVsString
        (takeBaseName file)                -- test name
        (replaceExtension file ".golden")  -- golden file path
        (goldenValue file)                 -- action whose result is tested
    | file <- files
    ]

goldenValue :: FilePath -> IO ByteString
goldenValue file = do
  checkFile defaultTabSize 1 {-verbose: -} True file >>= \case

    CheckIOError e ->
      ioError e

    CheckOK ->
      return "OK\n"

    CheckViolation _ errs ->
      return $ LazyText.encodeUtf8 $ LazyText.fromStrict $
        Text.unlines $ "Violations:" : map (displayLineError file) errs
