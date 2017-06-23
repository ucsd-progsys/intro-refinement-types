<div class="hidden">

\begin{code}
{-# LANGUAGE TupleSections    #-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--totality"      @-}
{-@ LIQUID "--diff"           @-}

module Termination where

import Prelude hiding (replicate, map, repeat)

fib, fib' :: Int -> Int
map :: (a -> b) -> [a] -> [b]

-- isOdd, isEven :: Int -> Bool
ack :: Int -> Int -> Int
range :: Int -> Int -> [Int]
replicate :: Int -> Int -> [Int]
isEven, isOdd :: Int -> Bool
\end{code}

</div>


Termination Checking
--------------------

<br>


Why termination Checking?
-------------------------

<br>
<br>

**By default, LH checks that functions terminate!**

Most functions _should_ terminate

For _soundness_ w.r.t. laziness [[ICFP 14]](http://dl.acm.org/citation.cfm?id=2628161)

<br>

Example: Termination of `fib`
-----------------------------

<br>
<br>
\begin{code}
{-@ fib :: i:Int -> Int  @-}
fib i | i == 0    = 0
      | i == 1    = 1
      | otherwise = fib (i-1) + fib (i-2)
\end{code}

<br>
<br>
**Q:** Why is there an error?

Proving Termination
-------------------

<br>

**Liquid Haskell Checks:**

Some _well founded metric_ decreases at each recursive call.

<br>

**Default Metric:**

The _first_ `Int` parameter.

Example: Termination of `fib`
-----------------------------

<br>
<br>
\begin{code}
{-@ fib' :: Int -> Int  @-}
fib' i | i <= 0    = 1
       | i == 1    = 1
       | otherwise = fib' (i-1) + fib' (i-2)
\end{code}

<br>
<br>
**Automatically Proved Terminating**



User Specified Termination Metrics
-----------------------------------

<br>

The first `Int` need not always be decreasing!

\begin{code}
{-@ replicate :: Int -> Int -> [Int] @-}
replicate _ n | n <= 0 = []
replicate x n = x : replicate x (n - 1)
\end{code}

**Specify metric as an expression over the inputs**

User Specified Termination Metrics
-----------------------------------

<br>

The first `Int` need not always be decreasing!

\begin{code}
{-@ range :: lo:Int -> hi:Int -> [Int] @-}
range lo hi
 | lo < hi   = lo : range (lo+1) hi
 | otherwise = []
\end{code}

**Excercise:** Fill in metric that proves `range` terminates.

Proving Termination
-------------------

<br>

**Liquid Haskell Checks:**

Some _well founded metric_ decreases at each recursive call.

<br>

Either _first_ `Int` parameter (default)

or

**User specified metric**


Lexicographic Termination
-------------------------

Why does [Ackermann Function](https://en.wikipedia.org/wiki/Ackermann_function) terminate?

\begin{code}
{-@ ack :: m:Int -> n:Int -> Int @-}
ack m n
  | m == 0    = n + 1
  | n == 0    = ack (m - 1) 1
  | otherwise = ack (m - 1) (ack m (n - 1))
\end{code}

First argument `m` decreases **or** second argument `n` decreases.

**Specify lexicographically ordered sequence of termination metrics** `[m, n]`


How About Data Types?
---------------------

<br>

Why does `map` terminate?

\begin{code}
{-@ map :: (a -> b) -> xs:[a] -> [b] / [len xs] @-}
map _ []     = []
map f (x:xs) = f x : map f xs
\end{code}

**Recursive Calls on Smaller Lists.**

Use first parameter with _associated size_

... as default metric.

User specified metrics on ADTs
------------------------------

<br>

What does `merge` terminate?

\begin{code}
{-@ merge :: xs:[a] -> ys:[a] -> [a] @-}
merge (x:xs) (y:ys)
  | x < y           = x:(merge xs (y:ys))
  | otherwise       = y:(merge ys (x:xs))
merge xs []         = xs
merge [] ys         = ys
\end{code}

**Exercise:** The default is insufficient here; can you fill in a suitable metric?

<!-- RJ: the mutually recursive stuff is pure black magic hackery, CUT.
     it ONLY makes sense with the GHOST (as in the README) and its not
     discussed here. In short, super confusing, hence, CUTTING.
-->

Mutually Recursive Functions
----------------------------

Same idea generalizes to mutual recursion.

\begin{code}
{-@ isEven :: n:Nat -> Bool  @-}
{-@ isOdd  :: m:Nat -> Bool  @-}

isEven 0 = True
isEven n = isOdd (n-1)

isOdd m = not $ isEven m
\end{code}

**Exercise:** Can you find the correct metric?

Liquid Haskell does not even attempt to guess it...


Diverging Functions
-------------------

<br>

**Some functions *do not* terminate!**


\begin{code}
{-@ lazy repeat @-}
repeat x = x : repeat x
\end{code}

`lazy` annotation deactivates termination checking.

Proving Termination
-------------------

<br>

**Liquid Haskell Checks**

Some _well founded metric_ decreases at each recursive call.

First `Int` or _sized_ parameter (default), *or*

User specified lexicographic metric, *or*

The function is marked `lazy`.

Termination is Easy in Practice
-------------------------------

<div align="center">

**Library**                     **LOC**     **Specs**      **Time**
---------------------------   ---------   -----------    ----------
`XMonad.StackSet`                   256            74          27s
`Data.List`                         814            46          26s
`Data.Set.Splay`                    149            27          27s
`Data.Vector.Algorithms`           1219            76          61s
`Data.Map.Base`                    1396           125          68s
`Data.Text`                        3128           305         231s
`Data.Bytestring`                  3505           307         136s
**Total**                     **11512**       **977**     **574s**
---------------------------   ---------   -----------    ----------

</div>

Termination is Easy in Practice
-------------------------------

<br>

**`503` Recursive Functions**

- `67%` via default metrics
- `30%` user given metrics
-  `1`  metric per `100` LOC


**`20` functions *not proven* to terminate**

- `12`  *do not* terminate (e.g. top-level `IO` loops)
- `8`   currently *outside scope* of LiquidHaskell



Recap
-----

<br>

|                     |                                |
|--------------------:|:-------------------------------|
| **Refinements:**    | Types + Predicates             |
| **Specification:**  | Refined Input/Output Types     |
| **Verification:**   | SMT-based Predicate Subtyping  |
| **Measures:**       | Specify Properties of Data     |
| **Termination:**    | Well-founded Metrics           |


What properties can be expressed in the logic?
----------------------------------------------

<br>

**Decidable SMT Theories**

- Boolean Propositions
- Linear Arithmetic
- Uninterpreted functions

<br>

**Next: _Any_ Terminating Haskell Function**

[Refinement Reflection](06-reflection.html)
