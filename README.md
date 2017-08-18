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

USING THE **arrowp-ext** PREPROCESSOR
---------------------------------

```
{-# OPTIONS -F -pgmF arrowp-ext #-}
```
