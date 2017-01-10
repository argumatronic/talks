% Applicative Parsing
% Julie Moronuki
% January 10, 2017

# Hour 1: Applicative

![Tie Fighter of Doom](vaders-tie-fighter.jpg)

# Functor

```haskell
fmap :: (a -> b) -> f a -> f b
```

# Monad

- ... is a kind of functor.

- but the `(a -> b)` of `fmap` has become an `(a -> f b)`

# fmap vs bind

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b

(>>=) :: Monad m  => m a -> (a -> m b) -> m b
```

# Join

```haskell
join :: Monad m => m (m a) -> m a
```

# Sequencing

- <read in book, how it depends on one thing (the m a?) to even know if the function will be applied
- with effectful code, sequencing is good

# Applicative  

![I have altered the Functor.](dog-vader.jpg)

Pray I do not alter it further.

# Applicative

```haskell
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```
- the two functions must be independent, not relying on each other for outcome  
- does not generate extra structure
- function application does not depend on result of earlier computation

# Applicatives vs Monads  
- context sensitivity 
- composability (applicatives compose; monads need transformers)  
- parallelism vs sequence

# Examples of monadic code and applicative code  
- AccValidation  
  - example

# AccValidation can't be a Monad 
  

# Applicative Do

# Parsing

# Monadic parsing  
- Parsec?

# Alternative

# Applicative parsing  
- usually context free due to the independent outcomes quality
- can also be used to parse context-sensitive grammars tho!  
- do not address this - reference to Yorgey's post about it

# Examples of monadic and applicative parsing  
- context free and context sensitive

# Hour 2: Electric Boogaloo

In this hour, we'll be working on a small project with the optparse-applicative library.
<!-- need to add instructions for Stack
probably have a small project (simple) initialized so we can load it up with samples
and also then clone the pprkpr repo, whatever we're going to call that
consider making rmbrfeed, todolist into a cla as well
 -->
# Example

-- optex 
-- stack exec optex

## Options.Applicative.Builder

Here are some basic argument types we can use: commands and flags.

```haskell
command :: String -> ParserInfo a -> Mod CommandFields a
```

Add a command to a subparser option.

```haskell
flag :: a  -> a -> Mod FlagFields a -> Parser a
--     [1]   [2]         [3]              [4]
```
1. default value
  
2. active value

3. option modifier
  
4. Builder for a flag parser.

A flag that switches from a "default value" to an "active value" when encountered. For a simple boolean value, use switch instead.

```haskell
switch :: Mod FlagFields Bool -> Parser Bool

switch = flag False True
```
-- flagEx.hs


subparser :: Mod CommandFields a -> Parser a

Builder for a command parser. The command modifier can be used to specify individual commands.

strArgument :: Mod ArgumentFields String -> Parser String

Builder for a String argument.

argument :: ReadM a -> Mod ArgumentFields a -> Parser a

Builder for an argument parser.



## information we can provide about our arguments

short :: HasName f => Char -> Mod f a

Specify a short name for an option.

long :: HasName f => String -> Mod f a

Specify a long name for an option.

metavar :: HasMetavar f => String -> Mod f a

Specify a metavariable for the argument.

Metavariables have no effect on the actual parser, and only serve to specify the symbolic name for an argument to be displayed in the help text.


## Options.Applicative.Extra

execParser :: ParserInfo a -> IO a Source #

Run a program description.

Parse command line arguments. Display help text and exit if any parse error occurs.

## a sample program

https://haskell-lang.org/library/optparse-applicative

import Options.Applicative

data Opts = Opts
    { optFlag :: !Bool
    , optVal :: !String
    }

main :: IO ()
main = do
    opts <- execParser optsParser
    putStrLn
        (concat ["Hello, ", optVal opts, ", the flag is ", show (optFlag opts)])
  where
    optsParser =
        info
            (helper <*> versionOption <*> programOptions)
            (fullDesc <> progDesc "optparse example" <>
             header
                 "optparse-example - a small example program for optparse-applicative")
    versionOption = infoOption "0.0" (long "version" <> help "Show version")
    programOptions =
        Opts <$> switch (long "some-flag" <> help "Set the flag") <*>
        strOption
            (long "some-value" <> metavar "VALUE" <> value "default" <>
             help "Override default name")

Without arguments, this program outputs Hello, default! The flag is False. If you run this program with the --help argument

-- data Opts = Opts { foo :: Bool, bar :: Bool } deriving Show

-- parseOpts :: Parser Opts
-- parseOpts = Opts <$> switch (long "foo" <> short 'f' <> help "Foo") <*> switch (long "bar" <> short 'b' <> help "Bar")

-- parseInfoCom :: ParserInfo Command
-- parseInfoCom = info parseCommand (progDesc "Give foo.")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- parseCommand :: Parser Command
-- parseCommand = subparser $
--     command "foo" (parseOpts `withInfo` "Give me a foo")

-- main :: IO ()
-- main = do
--     opts <- execParser parseInfoCom
--     print opts

# Start working on address
