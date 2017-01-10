So, we have functors, and that allows us to apply some function inside a container or wrapper or data structure. Or, in another sense, lets us lift the function into the context of the wrapper or container. 

And we have Monad, which is a type of functor. But in this case, the function being applied generates a new layer of structure itself. It's an (a -> m b) instead of an (a -> b), so that leaves us with two layers of structure we have to *join* and the composition of fmap and join gives us Monad's bind. 

<code: refactor.hs>

So, with Applicative, it's also a sort of functor. It's also lifting a function into a structured context -- or applying a function to the value(s) inside the container. But in this case, the function itself is wrapped (in contrast to fmap). In contrast to bind, it does not itself generate any new layer of structure. In fact, here the "structure" part of the operation gets monoidally appended. 

<code: refactor.hs>

# Sequencing

- <read in book, how it depends on one thing (the m a?) to even know if the function will be applied
- with effectful code, sequencing is good

# Applicative
- the two functions must be independent, not relying on each other for outcome  
- does not generate extra structure
- function application does not depend on result of earlier computation

# Examples of monadic code and applicative code 
<code: acc> 

# AccValidation can't be a Monad 
  

# Applicative Do

# Parsing

# Monadic parsing  
- Parsec?

# Applicative parsing  
- usually context free due to the independent outcomes quality

# Examples of monadic and applicative parsing  
- context free and context sensitive
