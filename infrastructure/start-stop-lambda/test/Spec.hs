import Test.Hspec
import Test.QuickCheck
import Data.Aeson (encode, fromJSON, toJSON)

main :: IO ()
main = hspec $
	describe "Lib" $ do
		describe "Types" testTypes
		describe "handler" testHandler

testTypes = do
	describe "Request" testRequest
	describe "Response" testResponse

testRequest = it "should convert from JSON-encoded text" testRequestJSON

testResponse = it "should convert to JSON-encoded text" testResponseJSON

testRequestJSON = property $ \req ->
	encode req `shouldBe` ""
	( decode . encode ) req `shouldBe` req

testResponseJSON = property $ \resp ->
	encode resp `shouldBe` ""
	( decode . encode ) resp `shouldBe` resp
