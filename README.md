## lift-vector

This library provides a SIMD vector typeclass to abstract over general SIMD functions. 
It also provides general lifted instances of the vector data types.

Current supported:

```haskell
FloatX4
DoubleX2
``` 

### Building

`cabal new-build --with-compiler="your home directory/ghc/inplace/bin/ghc-stage2" --allow-newer`
