{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Ackit.Compose (Compose (..))
import Ackit.Data (Text (..), VList (..))
import Ackit.Parse (Parse (..), splitFirstLine, splitFirstWord)
import GHC.Generics (Generic)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@=?), (@?=))

newtype QA1Text = QA1Text Text
  deriving (Eq, Ord, Show, Generic, Parse, Compose)

newtype QA1Int = QA1Int Int
  deriving (Eq, Ord, Show, Generic, Parse, Compose)

newtype QA1Double = QA1Double Double
  deriving (Eq, Ord, Show, Generic, Parse, Compose)

newtype QA1Char = QA1Char Char
  deriving (Eq, Ord, Show, Generic, Parse, Compose)

newtype QA1List = QA1List [Int]
  deriving (Eq, Ord, Show, Generic, Parse, Compose)

newtype QA1VList = QA1VList (VList Int)
  deriving (Eq, Ord, Show, Generic, Parse, Compose)

newtype QA1ListVList = QA1ListVList (VList [Double])
  deriving (Eq, Ord, Show, Generic, Parse, Compose)

newtype QA1TupleList = QA1TupleList [(Int, Int)]
  deriving (Eq, Ord, Show, Generic, Parse, Compose)

newtype QA1Tuple = QA1Tuple (Int, Double)
  deriving (Eq, Ord, Show, Generic, Parse, Compose)

data QA1IntOrDouble = QA1IDInt Int | QA1IDDouble Double
  deriving (Eq, Ord, Show, Generic, Parse, Compose)

data QA2 = QA2 Int Double
  deriving (Eq, Ord, Show, Generic, Parse, Compose)

data QA3 = QA3 Int Double Text
  deriving (Eq, Ord, Show, Generic, Parse, Compose)

data QA4 = QA4 Int Double Text [Int]
  deriving (Eq, Ord, Show, Generic, Parse, Compose)

data QA5 = QA5 Int Double Text [Int] Int
  deriving (Eq, Ord, Show, Generic, Parse, Compose)

suite :: TestTree
suite =
  testGroup
    "Test Suite"
    [ testGroup
        "parse check"
        [ testCase "splitFirstLine1" $ splitFirstLine "abc\n11\n22.0\n33\n" @?= ("abc", "11\n22.0\n33\n"),
          testCase "splitFirstLine2" $ splitFirstLine "11\n22.0\n33\n" @?= ("11", "22.0\n33\n"),
          testCase "splitFirstLine3" $ splitFirstLine "22.0\n33\n" @?= ("22.0", "33\n"),
          testCase "splitFirstLine4" $ splitFirstLine "33\n" @?= ("33", ""),
          testCase "splitFirstWord1" $ splitFirstWord "abc 11 22.0 33\n" @?= ("abc", "11 22.0 33\n"),
          testCase "splitFirstWord2" $ splitFirstWord "11 22.0 33\n" @?= ("11", "22.0 33\n"),
          testCase "splitFirstWord3" $ splitFirstWord "22.0 33\n" @?= ("22.0", "33\n"),
          testCase "splitFirstWord4" $ splitFirstWord "33\n" @?= ("33\n", ""),
          testCase "sampleQA1Text" $ parse "abc\n" @?= pure (QA1Text (Text "abc"), ""),
          testCase "sampleQA1Int" $ parse "11\n" @?= pure (QA1Int 11, ""),
          testCase "sampleQA1Double" $ parse "11.0\n" @?= pure (QA1Double 11.0, ""),
          testCase "sampleQA1Char" $ parse "11.0\n" @?= pure (QA1Char '1', "1.0\n"),
          testCase "sampleQA1List" $ parse "11 22 33\n" @?= pure (QA1List [11, 22, 33], ""),
          testCase "sampleQA1VList" $ parse "11\n22\n33\n" @?= pure (QA1VList (VList [11, 22, 33]), ""),
          testCase "sampleQA1ListVList" $ parse "11.1 22.2 33.3\n44.4 55.5 66.6\n77.7 88.8 99.9\n" @?= pure (QA1ListVList (VList [[11.1, 22.2, 33.3], [44.4, 55.5, 66.6], [77.7, 88.8, 99.9]]), ""),
          testCase "sampleQA1TupleList" $ parse "11 22 33 44 55 66\n" @?= pure (QA1TupleList [(11, 22), (33, 44), (55, 66)], ""),
          testCase "sampleQA1Tuple" $ parse "11 22.0\n" @?= pure (QA1Tuple (11, 22.0), ""),
          testCase "sampleQA1IntOrDouble" $ parse "22.0\n" @?= pure (QA1IDDouble 22, ""),
          testCase "sampleQA2" $ parse "11\n22.0\n" @?= pure (QA2 11 22.0, ""),
          testCase "sampleQA3" $ parse "11\n22.0\nabc\n" @?= pure (QA3 11 22.0 (Text "abc"), ""),
          testCase "sampleQA4" $ parse "11\n22.0\nabc\n33 44 55\n" @?= pure (QA4 11 22.0 (Text "abc") [33, 44, 55], ""),
          testCase "sampleQA5" $ parse "11\n22.0\nabc\n33 44 55\n66\n" @?= pure (QA5 11 22.0 (Text "abc") [33, 44, 55] 66, "")
        ],
      testGroup
        "compose check"
        [ testCase "sampleQA1Text" $ "abc" @=? compose (QA1Text (Text "abc")),
          testCase "sampleQA1Int" $ "11" @=? compose (QA1Int 11),
          testCase "sampleQA1Double" $ "11.0" @=? compose (QA1Double 11.0),
          testCase "sampleQA1Char" $ "1" @=? compose (QA1Char '1'),
          testCase "sampleQA1List" $ "11 22 33" @=? compose (QA1List [11, 22, 33]),
          testCase "sampleQA1VList" $ "11\n22\n33\n" @=? compose (QA1VList (VList [11, 22, 33])),
          testCase "sampleQA1ListVList" $ "11.1 22.2 33.3\n44.4 55.5 66.6\n77.7 88.8 99.9\n" @?= compose (QA1ListVList (VList [[11.1, 22.2, 33.3], [44.4, 55.5, 66.6], [77.7, 88.8, 99.9]])),
          testCase "sampleQA1TupleList" $ "11 22 33 44 55 66" @=? compose (QA1TupleList [(11, 22), (33, 44), (55, 66)]),
          testCase "sampleQA1Tuple" $ "11 22.0" @=? compose (QA1Tuple (11, 22.0)),
          testCase "sampleQA1IntOrDouble" $ "22.0" @=? compose (QA1IDDouble 22),
          testCase "sampleQA2" $ "11\n22.0" @=? compose (QA2 11 22.0),
          testCase "sampleQA3" $ "11\n22.0\nabc" @=? compose (QA3 11 22.0 (Text "abc")),
          testCase "sampleQA4" $ "11\n22.0\nabc\n33 44 55" @=? compose (QA4 11 22.0 (Text "abc") [33, 44, 55]),
          testCase "sampleQA5" $ "11\n22.0\nabc\n33 44 55\n66" @=? compose (QA5 11 22.0 (Text "abc") [33, 44, 55] 66)
        ]
    ]

main :: IO ()
main = defaultMain suite