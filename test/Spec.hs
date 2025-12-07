import qualified Day11Spec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day11" Day11Spec.spec
