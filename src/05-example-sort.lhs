
<div class="hidden">
\begin{code}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--no-termination" @-}

module Sort where

import Prelude hiding (sum, length, map, filter, foldr, foldr1)
import qualified Data.Set as S -- hiding (elems, insert)

insert, insertE :: (Ord a) => a -> [a] -> [a]
sort, sortE     :: (Ord a) => [a] -> [a]

split :: [a] -> ([a], [a])
merge :: Ord a => [a] -> [a] ->  [a]
msort :: Ord a => [a] -> [a]


{-@ measure length @-}
{-@ length :: [a] -> Nat @-}
length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs

-- insert :: x:_ -> xs:_ -> ListN a {1 + length xs} 
-- insertE :: x:_ -> xs:_ -> ListE a {addElem x xs} 
-- {-@ split :: xs:[a] -> {v:([a], [a]) | length (fst v) + length (snd v) = length xs} @-}
-- {-@ merge :: xs:[a] -> ys:[a] -> ListN a {length xs + length ys} @-} 
\end{code}

</div>

Case Study: Sorting Lists
------------------------- 

<br>
<br>

**Insertion Sort**

<br>

**Merge Sort**

Case Study: Sorting Lists
------------------------- 

<br>
<br>

<font color="#1569C7">**Insertion Sort**</font>

<br>

**Merge Sort**


Insertion Sort
--------------

<br>

\begin{spec}<div/>
            sort :: (Ord a) => List a -> List a
            sort []         = []
            sort (x:xs)     = insert x (sort xs)
            
            insert :: (Ord a) => a -> List a -> List a
            insert x []     = x : Emp
            insert x (y:ys)
              | x <= y      = x : y : ys
              | otherwise   = y : insert x ys
\end{spec}

Goal: Verified Insertion Sort
-----------------------------

<br>
<br>

**Specify & Verify that output of `sort`**

<div class="mybreak"><br></div>

Is the same **size** as the input 

Has the same **elements** as the input 

(Later) Is actually **sorted** in increasing order

Goal: Verified Insertion Sort
-----------------------------

<br>
<br>

**Specify & Verify that output of `sort`**

<div class="mybreak"><br></div>

<font color="#1569C7">Is the same **size** as the input</font>

Has the same **elements** as the input 

(Later) Is actually **sorted** in increasing order

Property 1: Size
----------------

<br>
<br>

**An Alias for lists of size N** 

\begin{code}
{-@ type ListN a N = {v:[a] | length v == N} @-}
\end{code}


Property 1: Size
----------------

<br>

\begin{code}
{-@ sort :: xs:[a] -> ListN a (length xs) @-}
sort []         = []
sort (x:xs)     = insert x (sort xs)

{-@ insert :: a -> xs:[a] -> [a] @-}
insert x []     = [x] 
insert x (y:ys)
  | x <= y      = x : y : ys
  | otherwise   = y : insert x ys
\end{code}

**Exercise:** Fix the type of `insert` so `sort` checks?

Goal: Verified Insertion Sort
-----------------------------

<br>
<br>

**Specify & Verify that output of `sort`**

<div class="mybreak"><br></div>

Is the same **size** as the input

<font color="#1569C7">Has the same **elements** as the input</font>

(Later) Is actually **sorted** in increasing order

Property 2: Permutation
-----------------------

<br>

Same size is all fine, how about **same elements** in output?


SMT Solvers Reason About Sets
-----------------------------

<br>

Hence, we can write *Set-valued* measures

<br>

Using the standard library `Data.Set` API for convenience

<br>

\begin{spec} <div/>
import qualified Data.Set as S
\end{spec}


Specifying A List's Elements
----------------------------

<br>

\begin{code}
{-@ measure elems @-}
elems :: (Ord a) => [a] -> S.Set a
elems []     = S.empty
elems (x:xs) = addElem x xs

{-@ inline addElem @-}
addElem :: (Ord a) => a -> [a] -> S.Set a
addElem x xs = S.union (S.singleton x) (elems xs)
\end{code}

<br>

`inline` lets us reuse (non-recursive) program functions in refinements

Property 2: Permutation
-----------------------

Lets verify that `sortE` returns the same set of elements:

\begin{code}
{-@ type ListE a S = {v:[a] | elems v = S} @-}

{-@ sortE :: xs:[a] -> ListE a (elems xs) @-}
sortE []     = []
sortE (x:xs) = insertE x (sortE xs)

{-@ insertE :: x:a -> xs:[a] -> [a] @-} 
insertE x []      = [x]
insertE x (y:ys)
  | x <= y        = x : y : ys
  | otherwise     = y : insertE x ys
\end{code}

**Exercise:** Can you fix the type for `insertE` so `sortE` verifies?

Goal: Verified Insertion Sort
-----------------------------

<br>
<br>

**Specify & Verify that output of `sort`**

<div class="mybreak"><br></div>

Is the same **size** as the input 

Has the same **elements** as the input

<font color="#1569C7">
(Later) Is actually **sorted** in increasing order
</font>


Case Study: Sorting Lists
------------------------- 

<br>
<br>

**Insertion Sort**

<br>

<font color="#1569C7">
**Merge Sort**
</font>


Exercise: MergeSort Size 
------------------------

\begin{code}
{-@ msort :: xs:[a] -> ListN a (length xs) @-}
msort []   = []
msort [x]  = [x]
msort xs   = let (xs1, xs2) = split xs
             in 
                 merge (msort xs1) (msort xs2)

{-@ split :: xs:[a] -> ([a], [a]) @-}
split (x:y:zs) = let (xs, ys) = split zs 
                 in 
                     (x:xs, y:ys) 
split xs       = (xs, [])

{-@ merge :: xs:[a] -> ys:[a] -> [a] @-} 
merge xs []         = xs
merge [] ys         = ys
merge (x:xs) (y:ys)
  | x <= y          = x : merge xs (y:ys)
  | otherwise       = y : merge (x:xs) ys
\end{code}


Exercise: MergeSort Permutation
-------------------------------

<br>
<br>
<br>

**Go back and modify the previous code to verify**

\begin{spec}<div/>
                {-@ msort :: xs:[a] -> ListE a (elems xs) @-}
\end{spec}

Plan
----

<br>

**Part I:** [Refinements 101](02-refinements.html)

Case Study: [Vector Bounds](03-example-vectors.html)

**Part II:** [Properties of Structures](04-data-properties.html)

Case Study: [Sorting](05-example-sort.html), **[Interpreter](06-example-interpreter.html)**

**Part III:** [Invariants of Data Structures](07-data-legal.html)

Case Study: [Sorting and Search Trees](08-example-sort.html)

**Part IV:** [Termination](10-termination.html) and [Correctness Proofs](11-reflection.html)

Case Study: [Optimizing Arithmetic Expressions](12-example-opt.html)