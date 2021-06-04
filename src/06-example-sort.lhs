
<div class="hidden">
\begin{code}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--no-termination" @-}

module Sort where

import Prelude hiding (sum, length, map, filter, foldr, foldr1)
import qualified Data.Set as S -- hiding (elems, insert)

infixr 5 :< 

sort, sortE :: (Ord a) => [a] -> OList a
insert, insertE :: (Ord a) => a -> OList a -> OList a

split :: [a] -> ([a], [a])
merge :: Ord a => OList a -> OList a -> OList a
msort :: Ord a => [a] -> OList a

{-@ measure length @-}
{-@ length :: [a] -> Nat @-}
length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs

{-@ measure olen @-}
{-@ olen :: OList a -> Nat @-}
olen :: OList a -> Int
olen Emp       = 0
olen (_ :< xs) = 1 + olen xs

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

Has values in increasing **order**

Has the same **size** as the input 

Has the same **elements** as the input 


Goal: Verified Insertion Sort
-----------------------------

<br>
<br>

**Specify & Verify that output of `sort`**

<div class="mybreak"><br></div>

<font color="#1569C7">Has values in increasing **order**</font>

Has the same **size** as the input 

Has the same **elements** as the input 

Property 1: Order
-----------------

<br>

Recall the type of *Ordered Lists*

<br>

\begin{code}
data OList a = Emp 
             | (:<) { oHd :: a, oTl :: OList a }

{-@ data OList a = Emp
                 | (:<) {oHd :: a, oTl :: OList {v:a | oHd <= v} }
  @-}
\end{code}

Property 1: Order
-----------------

<div class="mybreak"><br></div>

Ensure *order* by using Ordered lists (!)

<div class="mybreak"><br></div>


\begin{code}
{-@ sort :: xs:[a] -> OList a @-}
sort []         = Emp 
sort (x:xs)     = insert x (sort xs)

{-@ insert :: a -> xs:OList a -> OList a @-}
insert x Emp       = x :< Emp 
insert x (y :< ys)
  | y <= x         = x :< y :< ys
  | otherwise      = y :< insert x ys
\end{code}

**Oops, Can you spot the bug?**

Goal: Verified Insertion Sort
-----------------------------

<br>
<br>

**Specify & Verify that output of `sort`**

<div class="mybreak"><br></div>

Has values in increasing **order**

<font color="#1569C7">Has the same **size** as the input </font>

Has the same **elements** as the input 

Property 2: Size
----------------

<br>
<br>

**Lets define aliases for lists of size N** 

\begin{code}
{-@ type ListN  a N = {v:[a]     | len  v == N} @-}
{-@ type OListN a N = {v:OList a | olen v == N} @-}
\end{code}


Property 2: Size
----------------

<br>

**Exercise:** Can you go back and check that `sort` has the type 

<br>

\begin{spec}<div/>
             sort :: (Ord a) => xs:[a] -> OListN a (len xs)
\end{spec} 

<br>

What type should `insert` have?

Goal: Verified Insertion Sort
-----------------------------

<br>
<br>

**Specify & Verify that output of `sort`**

<div class="mybreak"><br></div>

Has values in increasing **order**

Has the same **size** as the input

<font color="#1569C7">Has the same **elements** as the input</font>


Property 3: Elements
--------------------

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


Specifying The Elements of a List
---------------------------------

<div class="mybreak"><br></div>

\begin{code}
{-@ measure elems @-}
elems :: (Ord a) => [a] -> S.Set a
elems []     = S.empty
elems (x:xs) = addElem x (elems xs)

{-@ measure oelems @-}
oelems :: (Ord a) => OList a -> S.Set a
oelems Emp       = S.empty
oelems (x :< xs) = addElem x (oelems xs)

{-@ inline addElem @-}
addElem :: (Ord a) => a -> S.Set a -> S.Set a
addElem x xs = S.union (S.singleton x) xs
\end{code}

`inline` lets us reuse (non-recursive) program functions in refinements

Property 3: Permutation
-----------------------

<br>
<br>

**Lets define aliases for lists with elements E** 

\begin{code}
{-@ type ListE  a S = {v:[a]    | elems v  == S} @-}
{-@ type OListE a S = {v:OList a| oelems v == S} @-}
\end{code}

Property 3: Permutation
-----------------------

<br>

Now verify that `sortE` returns the same set of elements:

\begin{code}
{-@ sortE :: xs:[a] -> OListE a (elems xs) @-}
sortE []     = Emp
sortE (x:xs) = insertE x (sortE xs)

{-@ insertE :: x:a -> xs:OList a -> OList a @-} 
insertE x Emp     = x :< Emp
insertE x (y :< ys)
  | x <= y        = x :< y :< ys
  | otherwise     = y :< insertE x ys
\end{code}

**Exercise:** Can you fix the type for `insertE` so `sortE` verifies?

Case Study: Sorting Lists
------------------------- 

<br>
<br>

**Insertion Sort**

<br>

<font color="#1569C7">
**Merge Sort**
</font>


Exercise: Fix specifications to verify `msort`
----------------------------------------------

\begin{code}
{-@ msort :: xs:[a] -> OListN a (len xs) @-}
msort []   = Emp
msort [x]  = x :< Emp
msort xs   = let (xs1, xs2) = split xs in 
             merge (msort xs1) (msort xs2)

{-@ split :: xs:[a] -> ([a], [a]) @-}
split (x:y:zs) = let (xs, ys) = split zs in 
                     (x:xs, y:ys) 
split xs       = (xs, [])

{-@ merge :: xs:OList a -> ys:OList a -> OList a @-} 
merge (x :< xs) (y :< ys)
  | x <= y          = x :< merge xs (y :< ys)
  | otherwise       = y :< merge (x :< xs) ys
merge xs  Emp       = xs
merge Emp ys        = ys
\end{code}


MergeSort: Permutation
----------------------

<br>
<br>
<br>

**Exercise:** Modify the previous code to verify that

\begin{spec}<div/>
            {-@ msort :: xs:[a] -> OListE a (elems xs) @-}
\end{spec}

<br>
<br>

[Continue...](00-plan.html)
