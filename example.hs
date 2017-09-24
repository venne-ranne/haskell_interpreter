import Control.Exception
import Control.Monad
import Test.HUnit

assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)

testPasses = TestCase $ assertException DivideByZero (evaluate $ 5 `div` 0)
testFails  = TestCase $ assertException DivideByZero (evaluate $ 5 `div` 1)

main = runTestTT $ TestList [ testPasses, testFails ]
