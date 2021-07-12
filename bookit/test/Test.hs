module Main (main) where

import qualified Hedgehog

main :: IO ()
main = do
  runHedgehogTests

runHedgehogTests :: IO ()
runHedgehogTests = do
  putStrLn "---- Running Hedgehog Tests ----"
  mapM_ Hedgehog.checkParallel hedgehogTests
  putStrLn "---- Completed Hedgehog Tests ----"

hedgehogTests :: [Hedgehog.Group]
hedgehogTests =
  []
