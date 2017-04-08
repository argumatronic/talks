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

```haskell
λ> (2 :| [3, 4]) <> (5 :| [6, 7])
2 :| [3,4,5,6,7]
```
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
https://gist.github.com/chris-martin/a4a69d1f938ffcda0a738419311d4431

- the tuple applicative:
```haskell
λ> ("hello ", (+8)) <*> ("julie", 14)
("hello julie",22)
```
- the behavior of the fst values in the tuples is determined by the `Monoid` constraint. 

```haskell
data (,) a b = (,) a b

instance Monoid a => Applicative ((,) a) where
    pure b = (mempty, b)
    (a, f) <*> (a', b) = (a `mappend` a', f b)
```


# Patterns

- boolean algebra, set theory are connected; some set theory operations are analogs of arithmetic operations

- conjunction 

|       | False | True  |
|-------|-------|-------|
| False | False | False |
| True  | False | True  |

- disjunction

|       | False | True  |
|-------|-------|-------|
| False | False | True  |
| True  | True  | True  |


# But what if it's `Maybe`?

- conjunction 

|         | Nothing | Just a  |
|---------|---------|---------|
| Nothing | Nothing | Nothing |
| Just a  | Nothing | Just a  |

- the `Just a` could be a choice of one or a combination of the two `a` values
- error propagating: possibly this isn't what you want
- but it is what you get in the `Maybe` applicative!

```haskell
λ> Just (*8) <*> Nothing
Nothing
```

- disjunction

|         | Nothing | Just a  |
|---------|---------|---------|
| Nothing | Nothing | Just a  |
| Just a  | Just a  | Just a  |

- error correcting: this is more typically what we want from a `Maybe` monoid










# Boolean algebra  
- relationship btw boolean algebra and set theory  
- set theory analogs to addition and multiplication are union and (something) and they correspond to disjunction and conjunction in boolean algebra  
- so, almost every type (or set) that forms a semigroup with one operation will do so with at least one other operation  
- Any set of sets closed under the set-theoretic operations forms a Boolean algebra with the join operator being union, the meet operator being intersection  
- in Haskell we handle this with newtypes to rename types to define that structure that has a set and a binary associative operation



