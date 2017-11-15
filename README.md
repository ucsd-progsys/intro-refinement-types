README
======

This repository has the materials for a 25 minute talk on Refinement Types,
with [LiquidHaskell](https://github.com/ucsd-progsys/liquidhaskell).

For longer versions, you may be interested in:

+ [2 Hr Workshop](http://ucsd-progsys.github.io/lh-workshop/)
+ [Tutorial](http://ucsd-progsys.github.io/liquidhaskell-tutorial/)


Running LiquidHaskell
---------------------

1. [Try Online][online]
2. [VM Image][]
3. [Build Locally][local]

[online]: (http://ucsd-progsys.github.io/intro-refinement-types)
[local]:(https://github.com/ucsd-progsys/liquidhaskell-tutorial/blob/master/src/01-intro.lhs#L170-L197)
[vm]: http://goto.ucsd.edu/~gridaphobe/LiquidHaskell.ova

Virtual Machine
---------------

This is also very easy, if you can manage the 2Gb download.

**Step 1** Download [this VM image][vm]

**Step 2** Choose your editor. For *emacs* do:

       tar -zxvf liquid-emacs.tgz

and for *Spacemacs* (a great Vim-Emacs hybrid) do:

       tar -zxvf liquid-spacemacs.tgz

**Step 3** Grab the source files from Github.

Build Slides
------------

To build rust-style html (in dist/_site)

     $ stack exec -- make html

To build reveal.js slides (in dist/_slides)

     $ stack exec -- make slides

Edit Slides
-----------

You can modify the following parameters:

1. **Server URL**: change `liquidserver` in `assets/templates/preamble.lhs`
2. **MathJax URL**: change the relevant link in `assets/templates/pagemeta.template`

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

Outline [25]
-------

+ 01-intro         [3]
+ 02-refinements   [6]
+ 03-examples      [9]
+ 04-abstracting   [4]
+ 05-concl         [3]


Outline [45]
-------

+ 01-intro         [5]
+ 02-refinements   [8]
+ 03-examples      [10]
+ 04-termination   [7]
+ 05-reflection    [7]
+ 06-concl         [5]


Avoiding Infinite Loops
-----------

* Another fun application: termination?

    dog-chasing-tail.gif

- sum
- sumTR  [show:     metric]
- range  [exercise: metric]
- ack
- map OR reverse/reverseTR
- merge
- mergeSort - bug?


sort :: Ord a => [a] -> [a]
sort []   = []
sort [x]  = [x]
sort xs   = merge (sort xs1) (sort xs2)
  where
    (xs1, xs2) = split xs

{-@ merge :: Ord a => xs:[a] -> ys:[a] -> [a] / [(len xs + len ys)] @-}
merge :: Ord a => [a] -> [a] ->  [a]
merge xs []         = xs
merge [] ys         = ys
merge (x:xs) (y:ys)
  | x <= y          = x : merge xs (y:ys)
  | otherwise       = y : merge (x:xs) ys

{-@ split :: xs:[a] -> Halves a xs @-}
split :: [a] -> ([a], [a])
split (x:(y:zs)) = (x:xs, y:ys) where (xs, ys) = split zs
split xs         = (xs, [])

{-@ type Halves a Xs = {v: (Half a Xs, Half a Xs) | len (fst v) + len (snd v) == len Xs} @-}
{-@ type Half a Xs  = {v:[a] | (len v > 1) => (len v < len Xs)}                          @-}

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
  n:Nat -> { sum n = sum n = (n * (n + 1) / 2) }

* Proofs      are Programs.

  sumTo // full-proof

  sumTo // PLE



```
{-@ LIQUID "--higherorder"                         @-}
{-@ LIQUID "--automatic-instances=liquidinstances" @-}

module Blank where

{- prop :: x:Int -> y:Int -> {v:Int | v = x * y} @-}
-- prop :: Int -> Int -> Int
-- prop x y = y * x

{- assume (*) :: (Num a) => x:a -> y:a -> {v:a | v = x * y} @-}

{-@ reflect sumTo @-}
{-@ sumTo :: Nat -> Nat @-}
sumTo :: Int -> Int
sumTo n
  | n == 0    = 0
  | otherwise = n + sumTo (n-1)

{-@ thm :: n:Nat -> { 2 * sumTo n == n * (n + 1) } @-}
thm :: Int -> ()
thm 0 = ()
thm n = thm (n - 1)

{-@ reflect fac        @-}
{-@ fac :: Nat  -> Nat @-}
fac :: Int -> Int
fac n
  | n == 0    = 1
  | otherwise = n * fac (n-1)

{-@ reflect facTR       @-}
{-@ facTR :: Nat -> Nat @-}
facTR :: Int -> Int
facTr n

{-@ prop :: _ -> { fac 5 == 120 } @-}
prop :: a -> ()
prop _ = ()

```
