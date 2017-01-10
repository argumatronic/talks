So, we have functors, and that allows us to apply some function inside a container or wrapper or data structure. Or, in another sense, lets us lift the function into the context of the wrapper or container. 

And we have Monad, which is a type of functor. But in this case, the function being applied generates a new layer of structure itself. It's an (a -> m b) instead of an (a -> b), so that leaves us with two layers of structure we have to *join* and the composition of fmap and join gives us Monad's bind. 

<code: refactor.hs>

So, with Applicative, it's also a sort of functor. It's also lifting a function into a structured context -- or applying a function to the value(s) inside the container. But in this case, the function itself is wrapped (in contrast to fmap). In contrast to bind, it does not itself generate any new layer of structure. In fact, here the "structure" part of the operation gets monoidally appended. We can look at that in more detail another time.



