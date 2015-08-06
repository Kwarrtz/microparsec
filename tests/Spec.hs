
module Main where

import Test.Hspec
import Test.QuickCheck

import Data.Char

import Text.Microparsec
import Text.Microparsec.Prim

main :: IO ()
main = hspec $ do
  describe "ParserT" $ do
    describe "runParser" $ do
      it "consumes input" $ property $
        \ a -> case (a :: String) of
                [] -> runParser anySymbol ([] :: String) == Nothing
                (x:xs) -> runParser anySymbol (x:xs) == Just (x :: Char, xs :: String)
    describe "return" $ do
      it "consumes no input" $ property $
        \ cs -> runParser (return ()) cs == Just ((),cs :: String)
      it "returns the value" $ property $
        \ a cs -> runParser (return a) cs == Just (a :: Char, cs :: String)
  describe "parser generators" $ do
    describe "character matching" $ do
      describe "whitespace" $ do
        it "matches a newline" $
          parse space "\n" `shouldBe` Just '\n'
        it "matches a space" $
          parse space " " `shouldBe` Just ' '
        it "matches only if is whitespace" $ property $
          charProp space isSpace
        it "matches many whitespace characters" $
          parse spaces "  \n\t\n " `shouldBe` Just "  \n\t\n "
        it "does not match an alphanumeric character" $
          parse space "a" `shouldBe` Nothing
      describe "digit" $ do
        it "matches only if is digit" $ property $
          charProp digit isDigit
      describe "alphanum" $ do
        it "matches only if is alpha" $ property $
          charProp alpha isAlpha
        it "matches only if is alphanum" $ property $
          charProp alphaNum isAlphaNum
    describe "satisfy" $ do
      it "fails on 'const False'" $ property $
        \ c -> parse (satisfy $ const False) [c :: Char] == Nothing
      it "succeeds on 'const True'" $ property $
        \ c -> parse (satisfy $ const True) [c :: Char] == Just c
  where charProp p f c = let r = parse p [c]
                       in if f c
                          then r == Just c
                          else r == Nothing

