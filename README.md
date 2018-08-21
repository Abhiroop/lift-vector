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


### Note
This library doesn't work with the head of GHC. You can build my modified branch here: https://github.com/Abhiroop/ghc-1/tree/wip/simd-ncg-support
