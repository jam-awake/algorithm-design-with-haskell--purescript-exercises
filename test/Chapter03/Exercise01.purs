module Test.Chapter03.Exercise01 where

import Prelude

import Data.FoldableWithIndex (forWithIndex_)
import Data.List (List(..), (:))
import Data.NonEmpty (NonEmpty(..))
import Data.Tuple (Tuple(..))
import Test.Chapter03.Code.AdtSymList as AdtSymList
import Test.Chapter03.Code.BookSymList as BookSymList
import Test.Chapter03.Code.CaseSymList as CaseSymList
import Test.Chapter03.Code.SymmetricList as SymmetricList
import Test.Chapter03.Code.SafishSymList as SafishSymList
import Test.Chapter01.Exercise12.SnocList (SnocList(..), (<:))
import Test.Chapter03.Code.UnsafeSymList as UnsafeSymmetricList
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Exercise 1" do
  let
    expectedList = "a" : "b" : "c" : "d" : Nil
    bookSymList =
      [ Tuple ("a" : Nil) ("d" : "c" : "b" : Nil)
      , Tuple ("a" : "b" : Nil) ("d" : "c" : Nil)
      , Tuple ("a" : "b" : "c" : Nil) ("d" : Nil)
      ]
    unsafeList =
      [ { front: "a" : Nil
        , back: "d" : "c" : "b" : Nil
        }
      , { front: "a" : "b" : Nil
        , back: "d" : "c" : Nil
        }
      , { front: "a" : "b" : "c" : Nil
        , back: "d" : Nil
        }
      ]
    safishList =
      [ { front: "a" : Nil
        , back: SnocNil <: "b" <: "c" <: "d"
        }
      , { front: "a" : "b" : Nil
        , back: SnocNil <: "c" <: "d"
        }
      , { front: "a" : "b" : "c" : Nil
        , back: SnocNil <: "d"
        }
      ]
    caseSymList =
      [ CaseSymList.Ends
          (NonEmpty "a" $ "b" : "c" : Nil)
          (NonEmpty "d" SnocNil)
      , CaseSymList.Ends
          (NonEmpty "a" $ "b" : Nil)
          (NonEmpty "d" $ SnocNil <: "c")
      , CaseSymList.Ends
          (NonEmpty "a" $ Nil)
          (NonEmpty "d" $ SnocNil <: "b" <: "c")
      ]
    symmetricList =
      [ SymmetricList.Ends "a" ("b" : "c" : Nil) SnocNil "d"
      , SymmetricList.Ends "a" ("b" : Nil) (SnocNil <: "c") "d"
      , SymmetricList.Ends "a" Nil (SnocNil <: "b" <: "c") "d"
      ]
    -- Unlike the cases above, there's only one way to represent this list
    -- in our ADT.
    adtSymList =
      AdtSymList.Ends
        "a"
        ( AdtSymList.Ends
            "b"
            AdtSymList.Empty
            "c"
        )
        "d"

  describe "BookSymmetricList" do
    forWithIndex_ bookSymList \idx l ->
      it ("Variant " <> show idx <> " should equal expected list") do
        BookSymList.toList l `shouldEqual` expectedList
        BookSymList.toList (BookSymList.fromList expectedList) `shouldEqual` expectedList
  describe "UnsafeSymmetricList" do
    forWithIndex_ unsafeList \idx l ->
      it ("Variant " <> show idx <> " should equal expected list") do
        UnsafeSymmetricList.toList l `shouldEqual` expectedList
        UnsafeSymmetricList.toList (UnsafeSymmetricList.fromList expectedList) `shouldEqual` expectedList
  describe "SafishSymmetricList" do
    forWithIndex_ safishList \idx l ->
      it ("Variant " <> show idx <> " should equal expected list") do
        SafishSymList.toList l `shouldEqual` expectedList
        SafishSymList.toList (SafishSymList.fromList expectedList) `shouldEqual` expectedList
  describe "CaseSymList" do
    forWithIndex_ caseSymList \idx l ->
      it ("Variant " <> show idx <> " should equal expected list") do
        CaseSymList.toList l `shouldEqual` expectedList
        CaseSymList.toList (CaseSymList.fromList expectedList) `shouldEqual` expectedList
  describe "SymmetricList" do
    forWithIndex_ symmetricList \idx l ->
      it ("Variant " <> show idx <> " should equal expected list") do
        SymmetricList.toList l `shouldEqual` expectedList
        SymmetricList.toList (SymmetricList.fromList expectedList) `shouldEqual` expectedList
  it "AdtSymList - only variant should equal expected list" do
    AdtSymList.toList adtSymList `shouldEqual` expectedList
    AdtSymList.toList (AdtSymList.fromList expectedList) `shouldEqual` expectedList
