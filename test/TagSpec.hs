module TagSpec where

import Data.Functor.Identity (Identity (..))
import Data.Tag (Tag, (:>))
import qualified Data.Tag as Tag
import qualified Data.Tag.Vec as Tag.Vec
import Data.Tag.Sum
import Data.Type.Equality ((:~:) (..))
import Test.Hspec

type T1 = [Int, Bool, Word, String, Char]

spec :: Spec
spec = do
  describe "rec" $ do
    it "smoke" $ do
      let v =
            Tag.Vec.replicate @T1 $
              Identity . \case
                Tag.This -> 5
                Tag.That t -> case t of
                  Tag.This -> True
                  Tag.That t -> case t of
                    Tag.This -> 6
                    Tag.That t -> case t of
                      Tag.This -> "hello"
                      Tag.That t -> case t of
                        Tag.This -> 'a'
                        Tag.That t -> Tag.absurd t
      Tag.Vec.lookup Tag.This v `shouldBe` Identity 5
      Tag.Vec.lookup (Tag.That Tag.This) v `shouldBe` Identity True
      Tag.Vec.lookup (Tag.That (Tag.That Tag.This)) v `shouldBe` Identity 6
      Tag.Vec.lookup (Tag.That (Tag.That (Tag.That Tag.This))) v `shouldBe` Identity "hello"
      Tag.Vec.lookup (Tag.That (Tag.That (Tag.That (Tag.That Tag.This)))) v `shouldBe` Identity 'a'
      Tag.Vec.lookup (Tag.inject @Bool) v `shouldBe` Identity True
      Tag.Vec.lookup (Tag.inject @Word) v `shouldBe` Identity 6
      Tag.Vec.forM_ v $ \tag x -> Tag.has @Show tag (print x)

  describe "tag" $ do
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
      let showX :: Sum T1 Identity -> String
          showX (t :=> x) = Tag.has @Show @T1 t (show x)
      showX (Tag.inject @Int @T1 :=> Identity 1) `shouldBe` show @(Identity Int) (Identity 1)
      showX (Tag.inject @Bool @T1 :=> Identity True) `shouldBe` show @(Identity Bool) (Identity True)
