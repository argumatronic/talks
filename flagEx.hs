-- from the `choose` package by Chris Martin 
-- https://github.com/chris-martin/choose/blob/master/choose-exe/src/Data/Random/Choose/Executable.hs

-- | The command-line arguments for 'main'.
data Args = ArgsNormal Args'
          | ArgsVersion

data Args' = Args'
    { _argN :: Maybe Int  -- ^ /n/, the number of items to choose.
    }

parserN :: Parser (Maybe Int)
parserN = optional $ Opt.argument read mod
  where
      read = Opt.auto :: Opt.ReadM Int
      mod = Opt.metavar "N" <> Opt.help help
      help = "Number of items to choose (default: "
             <> (show :: Int -> String) defaultN <> ")"

argsParser :: Parser Args
argsParser = versionParser <|> normalParser
  where
    versionParser = Opt.flag' ArgsVersion $
        Opt.short 'v' <> Opt.long "version" <> Opt.hidden
    normalParser = ArgsNormal . Args' <$> parserN
