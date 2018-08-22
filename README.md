## lift-vector

This library provides polymorphic SIMD functions. 
It also provides general lifted instances of the vector data types.

Current supported:

```haskell
FloatX4
DoubleX2
``` 

### Building

`cabal new-build --with-compiler="your home directory/ghc/inplace/bin/ghc-stage2" --allow-newer`


### Note
This library works off a cutting edge branch (sorry for the pun) which can be found here: https://github.com/Abhiroop/ghc-1/tree/wip/simd-ncg-support
