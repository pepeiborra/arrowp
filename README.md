[![Hackage](https://img.shields.io/hackage/v/arrowp-qq.svg)](https://hackage.haskell.org/package/arrowp-qq)
[![Stackage Nightly](http://stackage.org/package/arrowp-qq/badge/nightly)](http://stackage.org/nightly/package/arrowp-qq)
[![Travis Build Status](https://travis-ci.org/pepeiborra/arrowp-qq.svg)](https://travis-ci.org/pepeiborra/arrowp-qq)
ARROWP-QQ
==========
A preprocessor for arrow notation packaged by Jose Iborra,
based on the arrowp preprocessor developed by Ross Paterson <ross@soi.city.ac.uk>.

Notable features include support for GHC Haskell syntax and a
quasiquoter that can be used instead of the preprocessor.

Note that recent versions of GHC support this notation directly, and
give better error messages to boot. But the translation produced by GHC
is in some cases not as good as it could be.

Status
------

The parser cannot handle banana brackets for
control operators in arrow notation (the **proc** keyword in the original paper), 
due to a 
[limitation](https://github.com/haskell-suite/haskell-src-exts/issues/45) 
in haskell-src-exts. In order to use banana brackets, the recommendation
is to fall back to the GHC Arrows parser. 

Support for GHC Haskell notation inside arrow blocks is not complete, e.g.
multi-way-if and lambda case are unlikely to work as expected. If you run into 
one of these, please open an issue or vote for an existing one, as I plan to extend
the support on demand.

Using the `proc` quasi quoter
---------------------------

```
addA :: Arrow a => a b Int -> a b Int -> a b Int
addA f g = [proc| x -> do
		y <- f -< x
		z <- g -< x
		returnA -< y + z |]
```

Using the **arrowp-ext** preprocessor
---------------------------------

```
{-# OPTIONS -F -pgmF arrowp-ext #-}
```

Comparison with **arrowp**
-----------------------
**arrowp-qq** extends the original **arrowp** in three dimensions:
1. It replaces the `haskell-src` based parser with one based on `haskell-src-exts`, which handles most of GHC 8.0.2 Haskell syntax.
2. It provides not only a preprocessor but also a quasiquoter, which is a better option in certain cases.
3. It extends the desugaring to handle static conditional expressions (currently only if-then-else). Example:
```
proc inputs -> do
  results <- processor -< inputs
  if outputResultsArg
    then outputSink -< results
    else returnA -< ()
  returnA -< results
```
The standard **arrowp** (and GHC) desugaring for this code is:
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

Comparison with **GHC**
-----------------------
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
