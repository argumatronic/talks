module Main where

import Options.Applicative
import Data.Monoid

data Command = Add String String |
                Phone String String |
                Show String |
                Email String String
                deriving (Show, Eq)

parserAdd :: Parser Command
parserAdd = Add 
    <$> strArgument (metavar "FILENAME") 
    <*> strArgument (metavar "PERSON_NAME")

parserPhone :: Parser Command
parserPhone = Phone 
    <$> strArgument (metavar "FILENAME")
    <*> strArgument (metavar "PHONE_NUMBER")

parserShow :: Parser Command
parserShow = Show <$> strArgument (metavar "FILENAME")

parserEmail :: Parser Command 
parserEmail = Email 
    <$> strArgument (metavar "FILENAME")
    <*> strArgument (metavar "EMAIL_ADDRESS")

parserCommand :: Parser Command
parserCommand = subparser $ 
    command "add" (parserAdd `withInfo` "Add an entry.") <>
    command "email" (parserEmail `withInfo` "Add email address.") <>
    command "phone" (parserPhone `withInfo` "Add phone number.") <>
    command "show" (parserShow `withInfo` "Show record.")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
