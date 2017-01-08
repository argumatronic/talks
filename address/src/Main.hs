module Main where

import Options.Applicative

data Command = Add String String   | 
               Email String String |
               Phone String String |
               Show String  
               deriving (Eq, Show)

parserAdd :: Parser Command
parserAdd = Add <$> strArgument (metavar "FILENAME") <*> strArgument (metavar "PERSON_NAME")

parserEmail :: Parser Command
parserEmail = Email <$> strArgument (metavar "FILENAME") <*> strArgument (metavar "EMAIL_ADDRESS")

parserPhone :: Parser Command
parserPhone = Phone <$> strArgument (metavar "FILENAME") <*> strArgument (metavar "PHONE_NUMBER")

parserShow :: Parser Command
parserShow = Show <$> strArgument (metavar "FILENAME")

parserCommand :: Parser Command
parserCommand = subparser $ 
    command "add" (parserAdd `withInfo` "Add an entry.") <>
    command "email" (parserEmail `withInfo` "Add email address.") <>
    command "phone" (parserPhone `withInfo` "Add phone number.") <>
    command "show" (parserShow `withInfo` "Show record.")

parserInfoCommand :: ParserInfo Command
parserInfoCommand = info parserCommand (progDesc "Manage address book.")

main :: IO ()
main = do
    command <- execParser parserInfoCommand
    print command

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
