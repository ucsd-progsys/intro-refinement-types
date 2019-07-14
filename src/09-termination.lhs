<div class="hidden">

\begin{code}
{-# LANGUAGE TupleSections    #-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--short-names"    @-}
{-  LIQUID "--diff"           @-}

module Termination where

import Prelude hiding (reverse, sum, repeat)

ack     :: Int -> Int -> Int
range   :: Int -> Int -> [Int]
fastSum :: Int -> Int -> Int

-- {-@ split :: xs:[a] -> Halves a xs @-}
-- {-@ type Halves a Xs = {v: (Half a Xs, Half a Xs) | len (fst v) + len (snd v) == len Xs} @-}
-- {-@ type Half   a Xs = {v:[a] | (len Xs > 1) => (len v < len Xs)}                         @-}
\end{code}

</div>

Code is meant to be run...
--------------------------

\begin{spec}<div/>
      msort :: (Ord a) => [a] -> [a]
      msort []   = []
      msort xs   = let (xs1, xs2) = split xs in
                      merge (msort xs1) (msort xs2)
      
      split :: [a] -> ([a], [a]) 
      split (x:y:zs) = let (xs, ys) = split zs in 
                         (x:xs, y:ys) 
      split xs       = (xs, [])
      
      merge :: (Ord a) => [a] -> [a] -> [a]  
      merge xs []         = xs
      merge [] ys         = ys
      merge (x:xs) (y:ys)
        | x <= y           = x : merge xs (y:ys)
        | otherwise        = y : merge (x:xs) ys  
\end{spec}


Code is meant to be run...
--------------------------

<br>


Code is meant to be run...and finish!
-------------------------------------

<br>
<br>

\begin{spec}<div/>
                λ> :l msort.hs
                [1 of 1] Compiling Main             ( msort.hs, interpreted )
                Ok, modules loaded: Main.
                λ> msort []
                []
                λ> msort [1,3,2]
                
\end{spec}

<br>



Avoiding Infinite Loops
--------------------

<p align=center>
<img src="img/dog-chasing-tail.gif" height=350px/>
</p>


Lets prove termination!
-----------------------

<br>




Example: Proving Termination of `sum`
-------------------------------------

<br>
<br>

\begin{code}
sum :: Int -> Int
sum 0 = 0
sum n = n + sum (n - 1)
\end{code}

<br>
<br>

**Exercise:** (Why) _does_ this terminate?

Proving Termination
-------------------

<p align=center>
<img src="img/falling-termination.jpg" height=350px/>
</p>

Proving Termination
-------------------

<p align=center>
<img src="img/falling-termination.jpg" height=175px/>
</p>

**Termination: A well founded metric decreases at each recursive call**


Proving Termination
-------------------

<p align=center>
<img src="img/falling-termination.jpg" height=175px/>
</p>

**Termination: A well founded metric decreases at each recursive call**

Metric gets _strictly smaller_ 

Metric has a _lower bound_


Proving Termination
-------------------

<p align=center>
<img src="img/falling-termination.jpg" height=175px/>
</p>

**Termination: A well founded metric decreases at each recursive call**

Metric gets _strictly smaller_ and has a _lower-bound_

_e.g. 99 bottles, 98 bottles, ..., 0 bottles!_

Example: Proving Termination of `sum`
-------------------------------------

<br>

\begin{code}
sum' :: Int -> Int
sum' 0 = 0
sum' n = n + sum (n - 1)
\end{code}

**Default Metric**

_First_ `Int` parameter

**Exercise:** Always gets smaller, but what's the _lower bound_?


User Specified Termination Metrics
-----------------------------------

<br>

The first `Int` need not always be decreasing!


User Specified Termination Metrics
-----------------------------------

<br>

The first `Int` need not always be decreasing!

\begin{code}
{-@ fastSum :: Int -> Int -> Int @-}
fastSum total 0 = total
fastSum total n = fastSum (total + n) (n - 1)
\end{code}

**Exercise:** Can you specify metric as an expression over the inputs?

User Specified Termination Metrics
-----------------------------------

<br>

**Specify metric as an expression over the inputs**

\begin{code}
{-@ range :: lo:Int -> hi:Int -> [Int] @-}
range lo hi
  | lo < hi   = lo : range (lo + 1) hi
  | otherwise = []
\end{code}

**Exercise:** What metric proves `range` terminates?

Proving Termination
-------------------

<br>

**Type Checker Verifies**

A well founded metric decreases at each recursive call.

<br>

Either _first_ `Int` parameter (default)

or

**User specified metric**


Lexicographic Termination
-------------------------

<br>

Why does [Ackermann Function](https://en.wikipedia.org/wiki/Ackermann_function) terminate?

\begin{code}
{-@ ack :: m:Int -> n:Int -> Int @-}
ack 0 n = n + 1
ack m 0 = ack (m - 1) 1
ack m n = ack (m - 1) (ack m (n - 1))
\end{code}

First argument `m` decreases _or_ second argument `n` decreases.

**Specify lexicographically ordered sequence of termination metrics** `[m, n]`

How About Data Types?
---------------------

<br>

Why does `append` terminate?

\begin{code}
append           :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys
\end{code}

**Recursive Calls on Smaller Lists**

Default Metric: First parameter with _associated size_

User specified metrics on Data Types
------------------------------------

<br>

Why _does_ `merge` terminate?

\begin{code}
{-@ merge :: xs:[a] -> ys:[a] -> [a] / [len xs + len ys] @-}
merge (x:xs) (y:ys)
  | x < y           = x : merge xs (y : ys)
  | otherwise       = y : merge ys (x : xs)
merge xs []         = xs
merge [] ys         = ys
\end{code}

**Exercise:** The default is insufficient. What is a suitable metric?

Termination Can be Tricky
-------------------------

<br>

**Exercise:** What's going on with this merge-`sort`? Can you fix it?

\begin{code}
sort :: Ord a => [a] -> [a]
sort []      = []
sort xs      = let (ys, zs) = split xs 
               in 
                 merge (sort ys) (sort zs)


split :: [a] -> ([a], [a])
split (x:y:zs) = let (xs, ys) = split zs in 
                     (x:xs, y:ys) 
split xs       = (xs, [])
\end{code}

Avoiding Infinite Loops
-------------------

<br>

**Type Check Termination** 

Some well founded metric decreases at each recursive call.


Avoiding Infinite Loops
-------------------

<br>

**Type Check Termination** 

Some well founded metric decreases at each recursive call.

<br>

First `Int` or _sized_ parameter (default), *or*


Avoiding Infinite Loops
-------------------

<br>

**Type Check Termination** 

Some well founded metric decreases at each recursive call.

<br>

First `Int` or _sized_ parameter (default), *or*

User specified (lexicographic) metric, *or*

Avoiding Infinite Loops
-------------------

<br>

**Type Check Termination** 

Some well founded metric decreases at each recursive call.

<br>

First `Int` or _sized_ parameter (default), *or*

User specified (lexicographic) metric, *or*

The function is marked `lazy` (non-terminating).


Avoiding Infinite Loops is Easy (in Practice)
-----------------------------------------

<br>

<div class="hidden">

Avoiding Infinite Loops is Easy (in Practice)
-----------------------------------------

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

</div>


Avoiding Infinite Loops is Easy (in Practice)
---------------------------------------------

<br>

**`503` Recursive Functions**

`67%` via default metrics vs `30%` user given metrics

`1`  metric per `100` LOC


Avoiding Infinite Loops is Easy (in Practice)
---------------------------------------------

<br>

**`503` Recursive Functions**

`67%` via default metrics vs `30%` user given metrics

`1`  metric per `100` LOC

**`20` functions *not proven* to terminate**

`12`  *do not* terminate (e.g. top-level `IO` loops)

`8`   currently *outside scope* of type checker 

Plan
----

<br> 

**Part I:** [Refinements 101](02-refinements.html)

Case Study: [Vector Bounds](03-example-vectors.html)

**Part II:** [Properties of Structures](04-data-properties.html)

Case Study: [Sorting](05-example-sort.html), [Interpreter](06-example-interpreter.html)

**Part III:** [Invariants of Data Structures](07-data-legal.html)

Case Study: [Sorting actually Sorts](08-example-sort.html)

**Part IV:** [Termination](09-termination.html) and **[Correctness Proofs](10-reflection.html)**

Case Study: [Optimizing Arithmetic Expressions](11-example-opt.html)

