README
======

This repository has the materials various talks and tutorials using
[LiquidHaskell](https://github.com/ucsd-progsys/liquidhaskell).

+ [3 hr tutorial (CAV 2019)](http://ranjitjhala.github.io/CAV19-tutorial/)      -- branch `cav19`
+ [1 hr minute talk (PLDI 17)](http://ucsd-progsys.github.io/live/)             -- branch `retro`

You may also be interested in other versions

+ [2 hr Workshop (LambdaConf 15)](http://ucsd-progsys.github.io/lh-workshop/)

And a really long but hopefully fun tutorial

+ [Tutorial](http://ucsd-progsys.github.io/liquidhaskell-tutorial/)

Running LiquidHaskell
---------------------

1. [Try Online][online]
2. [VM Image][vm]
3. [Build Locally][local]

[online]: (http://ucsd-progsys.github.io/intro-refinement-types)
[local]:(https://github.com/ucsd-progsys/liquidhaskell-tutorial/blob/master/src/01-intro.lhs#L170-L197)
[vm]: http://goto.ucsd.edu/~gridaphobe/LiquidHaskell.ova

Virtual Machine
---------------

This is also very easy, if you can manage the 2Gb download.

**Step 1** Download [this VM image][vm]

**Step 2** Choose your editor. For *emacs* do:

```sh
tar -zxvf liquid-emacs.tgz
```

and for *Spacemacs* (a great Vim-Emacs hybrid) do:

```sh
tar -zxvf liquid-spacemacs.tgz
```

**Step 3** Grab the source files from Github.

Build Slides
------------

To build rust-style html (in dist/_site)

```sh
stack exec -- make html
```

To build reveal.js slides (in dist/_slides)

```sh
stack exec -- make slides
```

Edit Slides
-----------

You can modify the following parameters:

1. **Server URL**: change `liquidserver` in `assets/templates/preamble.lhs`
2. **MathJax URL**: change the relevant link in `assets/templates/pagemeta.template`

liquidserver:   "http://goto.ucsd.edu:8097/"

Outline [25]
------------

+ 01-intro         [3]
+ 02-refinements   [6]
+ 03-examples      [9]
+ 04-abstracting   [4]
+ 05-concl         [3]

Outline PLDI 17 [45]
--------------------

+ 01-intro         [5]
+ 02-refinements   [8]
+ 03-examples      [10]
+ 04-termination   [7]
+ 05-reflection    [7]
+ 06-concl         [5]

Outline CAV 19 [180]
--------------------

[see this](http://ucsd-pl.github.io/cse230/Hw4/Interpreter.html)

Part I

+ [*] 01-intro
+ [*] 02-refinements
+ [*] 03-example-vectors

Part II

+ [ ] 04-data-properties
+ [ ] 05-example-mergesort
+ [ ] 06-example-interpreter

Part III

+ [ ] 07-data-legal
+ [ ] 08-example-mergesort
+ [ ] 09-example-searchtree

Part IV

+ [ ] 10-termination
+ [ ] 11-reflection
+ [ ] 12-example-const
+ [ ] 13-example-searchtree


Proving Theorems
----------------

* Reflection

  { v:Proof | sum 3 ==  6 }
  { v:Proof | sum 4 == 10 }
  { v:Proof | sum 5 == 15 }

  { sum 3 ==  6 }
  { sum 4 == 10 }
  { sum 5 == 15 }

  Need the body blah blah?

* Proposition are Types

  Proposition
  Forall n in Nat. sum n = (n * (n + 1) / 2)

  Type
  n:Nat -> { sum n = (n * (n + 1) / 2) }

* Proofs      are Programs.

  sumTo // full-proof

  sumTo // PLE

```
{-@ LIQUID "--higherorder"                         @-}
{-@ LIQUID "--automatic-instances=liquidinstances" @-}

module Blank where

import Prelude hiding (sum)
import Language.Haskell.Liquid.ProofCombinators

{-@ assume (*) :: (Num a) => x:a -> y:a -> {v:a | v = x * y} @-}

{-@ reflect sum @-}
{-@ sum :: Nat -> Nat @-}
sum :: Int -> Int
sum n
  | n == 0    = 0
  | otherwise = n + sum (n-1)

{-@ thm :: n:Nat -> { 2 * sum n == n * (n + 1) } @-}
thm :: Int -> ()
thm 0 = ()
thm n = thm (n - 1)

{-@ sumPf :: n:Nat -> { 2 * sum n == n * (n + 1) } @-}
sumPf  :: Int -> ()
sumPf 0 =   2 * (sum 0)
        ==. 0
        *** QED
sumPf n =   2 * (sum n)
        ==. 2 * (n + sum (n-1))
        ==. 2 * (n + ((n - 1) * n)) ? sumPf (n-1)
        ==. n * (n + 1)
        *** QED
```



{-@ LIQUID "--higherorder"                         @-}
{-@ LIQUID "--automatic-instances=liquidinstances" @-}
{-@ LIQUID "--exact-data-cons"                     @-}

module Blank where

import Prelude hiding ((++))

import Language.Haskell.Liquid.ProofCombinators


{-@ reflect ++ @-}
{-@ infix ++ @-}
(++) :: [a] -> [a] -> [a]
[]  ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

{-@ reflect append @-}
append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

{-@ inline assoc @-}
assoc op x y z = (x `op` y) `op` z == x `op` (y `op` z)

{-@ thm :: xs:[a] -> ys:[a] -> zs:[a] -> { (xs ++ ys) ++ zs = xs ++ (ys ++ zs) } @-}
thm :: [a] -> [a] -> [a] -> ()

thm [] ys zs     = ([] ++ ys) ++ zs
                ==. ys ++ zs
                ==. [] ++ (ys ++ zs)
                *** QED
thm (x:xs) ys zs = ((x:xs) ++ ys) ++ zs
                ==. (x : (xs ++ ys)) ++ zs
                ==.  x : ((xs ++ ys) ++ zs)
                ==.  x : (xs ++ (ys ++ zs)) ? thm xs ys zs
                ==.  (x : xs) ++ (ys ++ zs)
                *** QED


Misc Links
----------

WBL Heaps

+ [HS+DT proof](https://github.com/jstolarek/dep-typed-wbl-heaps-hs/blob/master/src/TwoPassMerge/CombinedProofs.hs#L68)
+ [HS](https://github.com/jstolarek/dep-typed-wbl-heaps-hs/blob/master/src/TwoPassMerge/NoProofs.hs#L96)
+ [HS+Liquid](https://github.com/ucsd-progsys/liquidhaskell/blob/master/tests/pos/WBL.hs#L129)

Insert Sort

+ https://github.com/davidfstr/idris-insertion-sort/tree/master
+ http://www.enseignement.polytechnique.fr/informatique/INF551/TD/TD5/aux/Insert_Sort.v
+ https://github.com/goldfirere/singletons/blob/master/tests/compile-and-dump/InsertionSort/InsertionSortImp.hs

