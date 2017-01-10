module AltParsing where

import Control.Applicative
import Text.Trifecta

type NumberOrString =
  Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos =
      (Left <$> integer)
  <|> (Right <$> some letter)
-- some ~~ one or more

main = do
  print $ parseString (some letter) mempty a
  print $ parseString integer mempty b
  print $ parseString parseNos mempty a
  print $ parseString parseNos mempty b
  print $ parseString (many parseNos) mempty c
  print $ parseString (some parseNos) mempty c

Prelude> parseString (some integer) mempty "123"
Success [123]
Prelude> parseString (many integer) mempty "123"
Success [123]
Prelude> parseString (many integer) mempty ""
Success []
Prelude> parseString (some integer) mempty ""
Failure (interactive):1:1: error: unexpected
    EOF, expected: integer
<EOF>
^