module TagSpec where

import Data.Functor.Identity (Identity (..))
import Data.Tag (Tag, (:>))
import qualified Data.Tag as Tag
import Data.Tag.Sum
import Data.Type.Equality ((:~:) (..))
import Test.Hspec

type T1 = [Int, Bool, Word, String, Char]

spec :: Spec
spec = do
  describe "inject" $ do
    it "should inject" $ do
      Tag.inject @Int @T1 `shouldBe` Tag.This
      Tag.inject @Word @T1 `shouldBe` Tag.That (Tag.That Tag.This)
      Tag.inject @Char @T1 `shouldBe` Tag.That (Tag.That (Tag.That (Tag.That Tag.This)))

    it "should project" $ do
      let testing :: forall xs x. (Int :> xs, String :> xs, Bool :> xs) => Tag xs x -> x -> x
          testing t v = case Tag.project @Int @x @xs t of
            Just Refl -> do
              1 + v
            Nothing -> case Tag.project @String @x @xs t of
              Just Refl -> "hello " ++ v
              Nothing -> case Tag.project @Bool @x @xs t of
                Just Refl -> True
                Nothing -> v
      testing (Tag.inject @Int @T1) 1 `shouldBe` 2
      testing (Tag.inject @String @T1) "world" `shouldBe` "hello world"
      testing (Tag.inject @Bool @T1) False `shouldBe` True

    it "should pattern match" $ do
      let testing :: Tag T1 x -> x -> x
          testing t v = case t of
            Tag.This -> 1 + v
            Tag.That t -> case t of
              Tag.This -> True
              Tag.That t -> case t of
                Tag.This -> 2 + v
                Tag.That t -> case t of
                  Tag.This -> "hello " ++ v
                  Tag.That t -> case t of
                    Tag.This -> 'a'
                    Tag.That t -> Tag.absurd t
      testing (Tag.inject @Int @T1) 1 `shouldBe` 2
      testing (Tag.inject @String @T1) "world" `shouldBe` "hello world"
      testing (Tag.inject @Bool @T1) False `shouldBe` True
      testing (Tag.inject @Char @T1) 'b' `shouldBe` 'a'

    it "should have Has" $ do
      let showX :: Sum Identity T1 -> String
          showX (t :=> x) = Tag.has @Show @T1 t (show x)
      showX (Tag.inject @Int @T1 :=> Identity 1) `shouldBe` show @(Identity Int) (Identity 1)
      showX (Tag.inject @Bool @T1 :=> Identity True) `shouldBe` show @(Identity Bool) (Identity True)
