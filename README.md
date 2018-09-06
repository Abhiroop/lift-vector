## lift-vector

This library provides polymorphic SIMD functions. 
It also provides general lifted instances of the vector data types.

The major combinators provided by this library are:

```haskell
fold :: (a -> a -> a) -> (b -> b -> b) -> b -> t b -> b
zipVec ::
       (a -> a -> a) -> (b -> b -> b) -> t b -> t b -> t b
fmap :: (a -> a) -> (b -> b) -> t b -> t b
fromContainer :: t b -> [b]
```
It accepts a vector function as the first argument and the corresponding scalar function as the second argument.
All of the functions are also overloaded, so for multiplying all the  elements of a structure you should write

```haskell
fold (\x y -> x * y :: FloatX4) (\x y -> x * y :: Float) 1 <vector data structure here>
```

the *vector data structure* mentioned can be a 

- Vector List which is plainly a wrapper around a list
- Vector array which is a shape polymorphic unboxed array

All of the operations computed on these data structures using the above mentioned combinators would be implicitly vectorised. We hope to add a few more data structures like vectorised trees etc

Current supported vector types in GHC:

```haskell
FloatX4
DoubleX2
``` 

### Building

`cabal new-build --with-compiler="your home directory/ghc/inplace/bin/ghc-stage2" --allow-newer`


### Note
This library works off a cutting edge branch (sorry for the pun) which can be found here: https://github.com/Abhiroop/ghc-1/tree/wip/simd-ncg-support
