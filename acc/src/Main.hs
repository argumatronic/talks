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


main :: IO ()
main = do
    putStrLn "Please enter a word."
    firstWord <- getLine
    putStrLn "Please enter a second word."
    secondWord <- getLine
    let validAna = isAnagram 
                   <$> (maybeWord firstWord) 
                   <*> (maybeWord secondWord)
    display validAna

-- does not work because no Monad instance
-- main :: IO ()
-- main = do
--     putStrLn "Please enter a word."
--     firstWord <- getLine
--     putStrLn "Please enter a second word."
--     secondWord <- getLine
--     let validAna = do
--             first  <- maybeWord firstWord
--             second <- maybeWord secondWord
--             pure $ isAnagram first second
--     display validAna



-- Intuitively, it can't be made a monad because in a monad, the effect 
-- (i.e. the validation failures) of a computation can depend on previously 
-- bound results. But in the case of a failure, there is no result. So Either 
-- has no choice than to short-circuit to failure in that case, since there is 
-- nothing to feed to the subsequent functions on right-hand sides of (>>=)s.

-- This is in stark contrast to applicative functors, where the effect (in this 
--     case, the validation failures) cannot depend on other results, which is 
-- why we can get all validation failures without having to feed results (where 
--     would they come from?) from one computation to the other.