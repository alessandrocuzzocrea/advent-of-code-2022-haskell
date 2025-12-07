import Test.Hspec
import qualified Day01Spec
import qualified Day02Spec
import qualified Day03Spec
import qualified Day04Spec
import qualified Day05Spec
import qualified Day06Spec

main :: IO ()
main = hspec $ do
    describe "Day01" Day01Spec.spec
    describe "Day02" Day02Spec.spec
    describe "Day03" Day03Spec.spec
    describe "Day04" Day04Spec.spec
    describe "Day05" Day05Spec.spec
    describe "Day06" Day06Spec.spec
