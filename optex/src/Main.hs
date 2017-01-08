#!/usr/bin/env stack
-- stack --resolver lts-7.14 --install-ghc runghc

module Main where

import Options.Applicative as O
import Data.Semigroup ((<>))
-- import Control.Monad (replicateM_)
-- import Data.Char

data Welcome = Welcome { name :: String }

runWithOptions :: Welcome -> IO ()
  runWithOptions opts =
  putStrLn ("Enjoy the snow, " ++ name opts ++ "!")

main :: IO ()
main = execParser opts >>= runWithOptions
  where
    parser = Welcome <$> argument str (metavar "NAME")
    opts = info parser mempty







-- data Welcome = Welcome { name :: String}