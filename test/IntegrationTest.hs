module Main where

import Log
import Log.Backend.StandardOutput
import Log.Backend.StandardOutput.Bulk
import Log.Backend.ElasticSearch
import Test.ElasticSearch

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.List
import System.Environment
import System.Process
import System.Random
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test-simple-stdout"] -> do
      logger <- stdoutLogger
      runLogT "log-test-integration" logger $ logTrace_ "kaboozle"
        `finally` (liftIO $ waitForLogger logger)
    ["test-bulk-stdout"] -> do
      logger <- bulkStdoutLogger
      runLogT "log-test-integration" logger $ logTrace_ "kaboozle"
        `finally` (liftIO $ waitForLogger logger)
    ["test-elasticsearch"] -> do
      let config = defaultElasticSearchConfig
      logger <- elasticSearchLogger config randomIO
      runLogT "log-test-integration" logger $ logTrace_ "kaboozle"
        `finally` (liftIO $ waitForLogger logger)
    _ -> runTests

runTests :: IO ()
runTests = do
  path <- getExecutablePath
  let config = defaultElasticSearchConfig
  testConfig <- defaultElasticSearchTestConfig config
  defaultMain $ testGroup "Integration Tests" [
    testCase "Log messages are not lost (simple stdout back-end)" $ do
        out <- readProcess path ["test-simple-stdout"] ""
        assertBool "Output doesn't contain 'kaboozle'"
          $ "kaboozle" `isInfixOf` out,

    testCase "Log messages are not lost (bulk stdout back-end)" $ do
        out <- readProcess path ["test-bulk-stdout"] ""
        assertBool "Output doesn't contain 'kaboozle'"
          $ "kaboozle" `isInfixOf` out,

    testCase "Log messages are not lost (Elasticsearch back-end)" $ do
        callProcess path ["test-elasticsearch"]
        refreshTestIndex testConfig
        hits <- getNumHits testConfig "kaboozle"
        assertBool "Elasticsearch returned zero hits for 'kaboozle'"
          $ (hits > 0)

    ]
