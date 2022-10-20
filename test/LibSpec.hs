module LibSpec (spec) where

import Lib
import Prelude hiding (lookup)

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "BinaryTree (Tree a)" $ do
    it "has property: lookup bits (insert bits item tree) == (Just item)" $
      property $ \bits (item :: Int) (tree :: Tree Int) ->
        lookup bits (insert bits item tree) == (Just item)

    it "has property: insert is idempotent" $
      property $ \bits (item :: Int) (tree :: Tree Int) ->
        insert bits item tree == insert bits item (insert bits item tree)

  describe "cpsInsert" $ do
    it "cpsInsert bits item id == insert bits item Empty" $
      property $ \bits (item :: Int) ->
        cpsInsert bits item id == insert bits item Empty
