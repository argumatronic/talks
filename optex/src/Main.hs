#!/usr/bin/env stack
-- stack --resolver lts-7.14 --install-ghc runghc

module Main where

import Options.Applicative as Opt
import Data.Semigroup ((<>))
import Data.Char

data Welcome = Welcome { name :: String }

runWithOptions :: Welcome -> IO ()
runWithOptions opts =
  putStrLn ("Enjoy the snow, " ++ name opts ++ "!")

main :: IO ()
main = execParser opts >>= runWithOptions
 -- execParser :: ParserInfo a -> IO a
  where
    parser = Welcome <$> argument str (metavar "NAME")
    opts = info parser mempty
--  info :: Parser a -> InfoMod a -> ParserInfo a













-- data Welcome = Welcome { name :: String
--                        , excited :: Bool
--                        }

-- -- notice that a switch is a Bool type

-- runWithOptions :: Welcome -> IO ()
-- runWithOptions opts =
--   putStrLn $ transform $
--     "Enjoy the snow, " ++ name opts ++ "!"

--   where
--     transform = if excited opts then map toUpper else id
-- -- add the where clause to handle what to do if the options change

-- main :: IO ()
-- main = execParser opts >>= runWithOptions
--   where
--     parser = Welcome <$> argument str (metavar "NAME")
--                      <*> switch (short 'e' Opt.<>
--                                long "excited" Opt.<>
--                                help "Run in excited mode.")
--     opts = info parser mempty

-- -- have an argument str -- the metavar allows it to provide usage information to us

-- -- stack exec -- optex Julie -e