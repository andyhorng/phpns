module Main where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Control.Monad

import Parser

main :: IO ()
main = defaultMain [testSuite]

testSuite = testGroup "Parser"
            [
                testCase "php" $ testFile "test/fixtures/1-index.php" $ show $ Code $ Seq [],
                testCase "php" $ testFile "test/fixtures/2-namespace.php" $ show $ Code $ Seq [Namespace "Test\\NS\\Hello"],
                testCase "php" $ testFile "test/fixtures/3-use.php" $ show $ Code $ Seq [Use "Test\\NS\\Foobar"],
                testCase "php" $ testFile "test/fixtures/4-class.php" $ show $ Code $ Seq [Class "World" $ Seq []]
            ]

testFile :: FilePath -> String -> IO ()
testFile file expected = do
    res <- readFile file
    let Right result = parseString res
    (expected @=? (show result))
