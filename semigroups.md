# Introduction to Monoids!  
#### Bayhac
#### April 8. 2017
#### Julie Moronuki

# Well, actually  

- We're mostly going to be talking about semigroups.    
- We're not going to concern ourselves much with the typeclasses in Haskell but try to understand the concepts themselves.

# What is a semigroup?  
- Not less important than monoids!  
- A semigroup is an algebraic structure consisting of a set together with a binary associative operation.  


# Semigroup operation  
- Binary associative.  
- Canonically think of multiplicative operations, but they need not be.  
- There are other binary operations that can work here, depending on the sets.   
- The other most obvious is addition. 


# Some semigroups  
- `NonEmpty` lists under concatenation 
- the set of positive integers (no zero) under addition  
- the set of integers with `minimum`/`maximum`


# Monoid
- A monoid is a semigroup that adds an identity element.
- The identity element is relative to the operation and the set. 
- Identity doesn't change the value of whatever it's being combined with. 
- The set of natural numbers (with zero) under addition is a monoid because now we have zero to act as the identity.
- The set of lists that includes the empty list is a monoid under concatenation because the empty list acts as an identity value.

# When do we want identity values?

- JavaScript:
```JavaScript
Math.min() < Math.max() => false
```
- Why does it return false?  
- Haskell throws an exception instead.
- You can write Haskell that does a similar thing as the JS, changing it to a monoidal fold. 

- the tuple applicative:
```Haskell
λ> ("hello ", (+8)) <*> ("julie", 14)
("hello julie",22)
```
- the behavior of the fst values in the tuples is determined by the `Monoid` constraint. 

```haskell













# Boolean algebra  
- relationship btw boolean algebra and set theory  
- set theory analogs to addition and multiplication are union and (something) and they correspond to disjunction and conjunction in boolean algebra  
- so, almost every type (or set) that forms a semigroup with one operation will do so with at least one other operation  
- Any set of sets closed under the set-theoretic operations forms a Boolean algebra with the join operator being union, the meet operator being intersection  
- in Haskell we handle this with newtypes to rename types to define that structure that has a set and a binary associative operation



