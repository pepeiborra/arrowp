[![Hackage](https://img.shields.io/hackage/v/arrowp-qq.svg)](https://hackage.haskell.org/package/arrowp-qq)
[![Stackage Nightly](http://stackage.org/package/arrowp-qq/badge/nightly)](http://stackage.org/nightly/package/arrowp-qq)
[![Travis Build Status](https://travis-ci.org/pepeiborra/arrowp-qq.svg)](https://travis-ci.org/pepeiborra/arrowp-qq)

arrowp-qq
==========
A preprocessor (aka syntax-desugarer) for arrow notation 
based on the original `arrowp` developed by Ross Paterson <ross@soi.city.ac.uk>.

`arrowp-qq` extends the original `arrowp` in three dimensions:
1. It replaces the `haskell-src` based parser with one based on `haskell-src-exts`, which handles most of GHC 8.0.2 Haskell syntax.
2. It provides not only a preprocessor but also a quasiquoter, which is a better option in certain cases.
3. It extends the desugaring to handle static conditional expressions. See the semantics section below for more details.

Note: `arrowp-qq` provides an enhanced superset of the original `arrowp`'s functionality. One should have no reason to install both. Considering `arrowp` no longer builds under modern versions of GHC and Cabal/Stack, `arrowp-qq` should clearly be the more optimum package to install.

Both `arrowp` and `arrowp-qq` were originally developed during the days when GHC's `Arrows` extension was not fully mature--a time when `proc` syntax was not the norm. Of course, recent versions of GHC now support this notation directly, and give better error messages to boot. Unfortunately, GHC's `proc` notation desugarer is in some cases not as good as it could be. As such, `arrowp-qq`'s quasi-quoter can still be useful in producing slightly more optimized desugaring for performance-critical, arrowized applications.

The modern use cases of `arrowp-qq` are as follows:
- Viewing how your `proc` blocks (roughly) look like after desugaring for debugging/profiling purposes.
  - Via the `arrowp` executable.
  - NOTE: `arrowp-qq` is NOT guaranteed to produce the same desugaring as GHC (see limitations & semantics below).
- Optimizing the performance of your `proc` blocks via smarter desugaring.
  - Either through the `arrowp` executable OR (preferably) the provided quasiquoter.

Limitations
------

The parser cannot handle banana brackets for
control operators in arrow notation (the **proc** keyword in the original paper), 
due to a [limitation](https://github.com/haskell-suite/haskell-src-exts/issues/45) 
in `haskell-src-exts`. In order to use banana brackets, the recommendation
is to fall back to the GHC Arrows parser.

Support for GHC Haskell notation inside arrow blocks is not complete, e.g.
multi-way-if and lambda case are unlikely to work as expected. If you run into 
one of these, please open an issue or vote for an existing one, as I plan to extend
the support on demand.

Installation
------------
`arrowp-qq` is now compatible with Cabal v3's Nix-style local builds & installs:

`cabal install arrowp-qq`

Usage 
-----
### Viewing desugared `proc` syntax
```sh
arrowp myfile.hs | less
```
### Optimization
#### Via the `proc` quasiquoter

```
addA :: Arrow a => a b Int -> a b Int -> a b Int
addA f g = [proc| x -> do
		y <- f -< x
		z <- g -< x
		returnA -< y + z |]
```

#### Via the **arrowp** preprocessor
Add the following GHC pragma to the top of the source file:
```
{-###  OPTIONS -F -pgmF arrowp ### -}
```
This can be useful for preserving compatibility with vanilla `proc` notation, at the cost of flexibility; that is to say, all `proc` blocks within the source file will be desugared via `arrowp-qq`.

Desugaring Semantics
-----------------------
### Static conditional expression optimization
As mentioned previously, `arrowp-qq` extends the original `arrowp`'s desugaring to handle static conditional expressions. Given:
```
proc inputs -> do
  results <- processor -< inputs
  if outputResultsArg
    then outputSink -< results
    else returnA -< ()
  returnA -< results
```
The standard `arrowp` (and GHC) desugaring for this code is:
```
  = ((processor >>> arr (\ results -> (results, results))) >>>
       (first
          (arr
             (\ results -> if outputResultsArg then Left results else Right ())
             >>> (outputSink ||| returnA))
          >>> arr (\ (_, results) -> results)))
```
This requires an `ArrowChoice`, but there is a more efficient desugaring which 
performs the choice at compile time and thus an `Arrow` suffices:
```
((processor >>> arr (\ results -> (results, results))) >>>
       (first
          (if outputResultsArg then outputSink else arr (\ results -> ()))
          >>> arr (\ (_, results) -> results)))
```

### `first` call optimization
The GHC desugarer does not do a very good job of minimizing the number of
`first` calls inserted. In certain `Arrow` instances, this can have a material effect
on performance. Example:
```
trivial = proc inputs -> do
   chunked <- chunk -< inputs
   results <- process -< chunked
   returnA -< results
```
This code ought to desugar to a chain of arrows, and indeed, both arrowp and
arrowp-qq desugar this to:
```
trivial = chunk >>> process
```
However GHC will produce (approximately) the following code:
```
  arr(\inputs -> (inputs,inputs)) >>> first chunk >>> first process >>> arr fst
```
