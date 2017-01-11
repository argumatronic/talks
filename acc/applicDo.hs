{-# LANGUAGE ApplicativeDo #-}

module Main where

import Data.List (null, sort)
import Data.Char (isAlpha)
import Data.Validation

isAnagram :: String -> String -> Bool
isAnagram xs ys = sort xs == sort ys

maybeWord :: String -> AccValidation [String] String
maybeWord xs = 
    case null xs of
        True -> AccFailure ["Empty string."]
        False -> do
            case (all isAlpha xs) of
                False -> AccFailure ["This is not a word."]
                True -> AccSuccess xs

display :: AccValidation [String] Bool -> IO ()
display validAna =
    case validAna of
        AccFailure err   -> putStrLn (concat err)
        AccSuccess False -> putStrLn "These are not anagrams."
        AccSuccess True  -> putStrLn "These words are anagrams."


validAna :: String -> String -> AccValidation [String] Bool
validAna xs ys = do
    first <- maybeWord xs
    second <- maybeWord ys
    pure (isAnagram first second)


main :: IO ()
main = do
    putStrLn "Please enter a word."
    firstWord <- getLine
    putStrLn "Please enter a second word."
    secondWord <- getLine
    display (validAna firstWord secondWord)