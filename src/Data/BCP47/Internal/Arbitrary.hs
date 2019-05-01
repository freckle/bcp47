module Data.BCP47.Internal.Arbitrary
  ( Arbitrary
  , arbitrary
  , alphaString
  , alphaNumString
  , alphaChar
  , numChar
  , choose
  , oneof
  , suchThat
  ) where

import Test.QuickCheck
  (Arbitrary, Gen, arbitrary, choose, elements, oneof, suchThat, vectorOf)

numChar :: Gen Char
numChar = elements numChars

alphaNumString :: Int -> Gen String
alphaNumString n = vectorOf n alphaNumChar

alphaString :: Int -> Gen String
alphaString n = vectorOf n alphaChar

alphaNumChar :: Gen Char
alphaNumChar = elements alphaNumChars

alphaChar :: Gen Char
alphaChar = elements alphaChars

alphaNumChars :: String
alphaNumChars = alphaChars ++ numChars

alphaChars :: String
alphaChars = ['a' .. 'z'] ++ ['A' .. 'Z']

numChars :: String
numChars = ['0' .. '9']
