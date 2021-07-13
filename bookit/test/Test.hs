module Main (main) where

import qualified Bookit.Sexp.ParseTest (tests)
import qualified Hedgehog
import qualified Test.DocTest

main :: IO ()
main = do
  runHedgehogTests
  runDocTests

runHedgehogTests :: IO ()
runHedgehogTests = do
  putStrLn "\n---- Running Hedgehog Tests ----"
  mapM_ Hedgehog.checkParallel hedgehogTests
  putStrLn "---- Completed Hedgehog Tests ----"

runDocTests :: IO ()
runDocTests = do
  putStrLn "\n---- Running DocTests ----"
  docTests
  putStrLn "---- Completed DocTests ----"

hedgehogTests :: [Hedgehog.Group]
hedgehogTests =
  [ Bookit.Sexp.ParseTest.tests
  ]

docTests :: IO ()
docTests =
  Test.DocTest.doctest
    [ "-isrc",
      "src/Bookit/Sexp/Char.hs",
      "src/Bookit/Sexp/Parse.hs",
      "src/Bookit/Sexp/Ppr.hs",
      "src/Bookit/Sexp/Types.hs"
    ]
