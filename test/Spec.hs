import Test.Tasty
import Tests

main :: IO()
main = defaultMain $ testGroup "Tests" [tests]
