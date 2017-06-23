<div class="hidden">

\begin{code}
{-# LANGUAGE TupleSections    #-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--totality"       @-}
{-@ LIQUID "--diff"           @-}

module DataTypes where

import Prelude hiding (replicate, (++), sum, init, length, map, filter, foldr, foldr1)

map         :: (a -> b) -> List a -> List b
foldr1      :: (a -> a -> a) -> List a -> a
head        :: List a -> a
tail        :: List a -> List a
init, init' :: (Int -> a) -> Int -> List a
-- append      :: List a -> List a -> List a
-- filter      :: (a -> Bool) -> List a -> List a
impossible         :: String -> a
average     :: List Int -> Int
-- wtAverage   :: List (Int, Int) -> Int

infixr 9 :::

{-@ data List [size] a = Emp | (:::) {hd :: a, tl :: List a } @-}
{-@ invariant {v: List a | 0 <= size v} @-}

{-@ type Nat      = {v:Int | v >= 0} @-}
{-@ type Pos      = {v:Int | v >  0} @-}

{-@ impossible :: {v:_ | false} -> a @-}
impossible = error

{-@ average :: ListNE Int -> Int @-}
average xs = total `div` n
  where
    total   = foldr1 (+) xs
    n       = size xs
\end{code}

</div>



Data Types
----------

<br>


Example: Lists
--------------

<br>

<div class="fragment">
Lets define our own `List` data type:

<br>

\begin{code}
data List a = Emp               -- Nil
            | (:::) a (List a)  -- Cons
\end{code}
</div>


Specifying the size of a List
-------------------------------

<br>

<div class="fragment">
**Measure**

Haskell function with *a single equation per constructor*
</div>

\begin{code}
{-@ measure size @-}
size :: List a -> Int
size Emp        = 0
size (_ ::: xs) = 1 + size xs
\end{code}



Specifying the size of a List
-------------------------------

**Measure**

*Strengthens* type of data constructor

<div class="fragment">

\begin{spec} <div/>
data List a where
  Emp   :: {v:List a | size v = 0}
  (:::) :: x:a -> xs:List a -> {v:List a|size v = 1 + size xs}
\end{spec}
</div>


Using Measures
---------------

<br>





Exercise: *Partial* Functions
-----------------------------

<br>

Fear `head` and `tail` no more!

<div class="fragment">
\begin{code}
{-@ head        :: List a -> a @-}
head (x ::: _)  = x
head _          = impossible "head"

{-@ tail        :: List a -> List a @-}
tail (_ ::: xs) = xs
tail _          = impossible "tail"
\end{code}

**Q:** Write types for `head` and `tail` that verify `impossible`.
</div>


Naming Non-Empty Lists
----------------------

<br>

A convenient *type alias*

<br>

\begin{code}
{-@ type ListNE a = {v:List a| 0 < size v} @-}
\end{code}

<br>

<div class="slideonly">

`head` and `tail` are Safe
--------------------------

When called with *non-empty* lists:

<br>

\begin{spec}
{-@ head :: ListNE a -> a @-}
head (x ::: _)  = x
head _          = impossible "head"

{-@ tail :: ListNE a -> List a @-}
tail (_ ::: xs) = xs
tail _          = impossible "tail"
\end{spec}

</div>

<br>




A Useful Partial Function: Fold / Reduce
----------------------------------------

<br>

**Fold** or **Reduce** `f` over *any* list using seed `acc`

<br>

\begin{code}
foldr :: (a -> b -> b) -> b -> List a -> b
foldr _ acc Emp        = acc
foldr f acc (x ::: xs) = f x (foldr f acc xs)
\end{code}

<br>

A Useful Partial Function: Fold / Reduce
----------------------------------------

<br>

**Fold** or **Reduce** `f` over *non-empty* list using *first element* as seed

<br>

\begin{code}
{-@ foldr1 :: (a -> a -> a) -> List a -> a @-}
foldr1 f (x ::: xs) = foldr f x xs
foldr1 _ _          = impossible "foldr1"
\end{code}

<br>

**Q:** How shall we fix `foldr1`?


Exercise: `average`
-------------------

<br>

\begin{code}
{-@ average' :: List Int -> Int @-}
average' xs = total `div` n
  where
    total   = foldr1 (+) xs
    n       = size xs
\end{code}

<br>

**Q:** What is a safe input type for `average'`?

<br>



Refining Data Types
-------------------


<br>
<br>

&nbsp; &nbsp; *Make illegal states unrepresentable*

<br>

Example: Year is 12 Months
--------------------------

<br>

\begin{code}
data Year a = Year (List a)
\end{code}

<br>

<div class="fragment">
**Legal Values:** Lists of `12` elements, e.g.

<br>

`"jan" ::: "feb" ::: ... ::: "dec" ::: Emp"`

</div>

<br>


Example: Year is 12 Months
--------------------------

<br>

**Refine Type to Legal Values**

\begin{code}
{-@ data Year a = Year (ListN a 12) @-}
\end{code}


**Lists Of A Given Size**

\begin{code}
{-@ type ListN a N = {v: List a | size v == N} @-}
\end{code}


<div class="fragment">
**Make illegal states unrepresentable**

\begin{code}
badYear = Year (1 ::: Emp)
\end{code}
</div>


Exercise: `map`
---------------

<br>

\begin{code}
{-@ map :: (a -> b) -> xs:List a -> List b @-}
map _ Emp         = Emp
map f (x ::: xs)  = f x ::: map f xs
\end{code}

<div class="fragment">
**Q:** Can you fix `map` to verify `tempAverage`?

\begin{code}
data Weather = W { temp :: Int, rain :: Int }

tempAverage :: Year Weather -> Int
tempAverage (Year ms) = average months
  where
    months            = map temp ms
\end{code}
</div>


Exercise: `init`
----------------

<br>

\begin{code}
{-@ init :: (Int -> a) -> Nat -> List a @-}
init _ 0 = Emp
init f n = f n ::: init f (n-1)
\end{code}

<br>

<div class="fragment">
**Q:** Can you fix the type of `init` so that `sanDiegoTemp` is accepted?

\begin{code}
sanDiegoTemp :: Year Int
sanDiegoTemp = Year (init (const 72) 12)
\end{code}
</div>


Exercise: `init'`
------------------

<br>

\begin{code}
{-@ init' :: (Int -> a) -> n:Nat -> List a @-}
init' f n = go 0
  where
    go i | i < n     = f i ::: go (i+1)
         | otherwise = Emp
\end{code}

<div class="fragment">
**Q:** For bonus points, fix `init'` so `sanDiegoTemp'`is accepted?

\begin{code}
sanDiegoTemp' :: Year Int
sanDiegoTemp' = Year (init' (const 72) 12)
\end{code}
</div>

Recap
-----

<br>

|                     |                                |
|--------------------:|:-------------------------------|
| **Refinements:**    | Types + Predicates             |
| **Specification:**  | Refined Input/Output Types     |
| **Verification:**   | SMT-based Predicate Subtyping  |
| **Measures:**       | Specify Properties of Data     |

<br>

<div class="fragment">
**Case Study:**  [Insertion Sort](04-case-study-insertsort.html)

+ [Well Scoped Evaluator](05-case-study-eval.html)
+ [Low-level Memory](06-case-study-bytestring.html)
</div>
