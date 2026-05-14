{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
A tiny golden-file helper. Compares actual output against a reference
committed to @test/golden/@. Set @GRANITE_BLESS_GOLDEN=1@ to overwrite
the reference with the actual output (useful when you intentionally
change the renderer).

Designed for the granite dep budget (base + text only): no
tasty-golden, no hspec-golden, no system-filepath.
-}
module Golden (
    goldenText,
    goldenDir,
) where

import Control.Exception (IOException, try)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Test.Hspec

goldenDir :: FilePath
goldenDir = "test/golden"

{- | Compare @actual@ against the file at @goldenDir/name@.

  * If the file is missing → the test fails with a clear hint.
  * If @GRANITE_BLESS_GOLDEN=1@ → write @actual@ to disk and pass.
  * Otherwise compare byte-for-byte.
-}
goldenText :: FilePath -> Text -> Expectation
goldenText name actual = do
    let path = goldenDir <> "/" <> name
    blessing <- lookupEnv "GRANITE_BLESS_GOLDEN"
    case blessing of
        Just "1" -> do
            Text.IO.writeFile path actual
            hPutStrLn stderr ("blessed: " <> path)
        _ -> do
            r <- try (Text.IO.readFile path) :: IO (Either IOException Text)
            case r of
                Left _ ->
                    expectationFailure $
                        "Missing golden file "
                            <> path
                            <> ".\nRun with GRANITE_BLESS_GOLDEN=1 to create it."
                Right expected -> do
                    let aLines = Text.lines actual
                        eLines = Text.lines expected
                    if actual == expected
                        then pure ()
                        else
                            expectationFailure $
                                "Golden mismatch in "
                                    <> path
                                    <> "\nFirst diverging line "
                                    <> firstDiff aLines eLines
                                    <> "\n(Re-bless with GRANITE_BLESS_GOLDEN=1 if the change is intentional.)"

firstDiff :: [Text] -> [Text] -> String
firstDiff as es = go 0 as es
  where
    go _ [] [] = "(unknown — file lengths match but bytes differ)"
    go i [] (e : _) = " (line " <> show i <> ": actual EOF, expected " <> Text.unpack e <> ")"
    go i (a : _) [] = " (line " <> show i <> ": actual " <> Text.unpack a <> ", expected EOF)"
    go i (a : as') (e : es')
        | a == e = go (i + 1) as' es'
        | otherwise =
            " (line "
                <> show i
                <> ":\n  actual: "
                <> Text.unpack a
                <> "\n  expected: "
                <> Text.unpack e
                <> ")"
