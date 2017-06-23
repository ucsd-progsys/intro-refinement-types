
<div class="hidden">
\begin{code}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--no-termination" @-}

module InsertSort where

import Prelude hiding (sum, length, map, filter, foldr, foldr1)
import qualified Data.Set as S -- hiding (elems, insert)

insert, insertE :: (Ord a) => a -> List a -> List a
sort, sortE     :: (Ord a) => List a -> List a

{-@ measure length @-}
length :: List a -> Int
length Emp        = 0
length (_ ::: xs) = 1 + length xs


data List a  = Emp
             | (:::) { hd :: a, tl :: List a }
             deriving (Eq, Ord, Show)

infixr 9 :::

infixr 9 :<:

-- | Lists of a given size N
{-@ type ListN a N = {v:List a | length v == N } @-}

{-@ type OListE a S = {v:OList a | elemsO v = S} @-}

{-@ measure elemsO @-}
elemsO :: (Ord a) => OList a -> S.Set a
elemsO OEmp       = S.empty
elemsO (x :<: xs) = addElemO x xs

{-@ inline addElemO @-}
addElemO :: (Ord a) => a -> OList a -> S.Set a
addElemO x xs = S.singleton x `S.union` elemsO xs
\end{code}

</div>

<br>

Case Study: Insertion Sort
--------------------------

<br>

Insertion Sort {#asdisort}
--------------

<br>

\begin{spec}
sort :: (Ord a) => List a -> List a
sort []           = Emp
sort (x:xs)       = insert x (sort xs)

insert :: (Ord a) => a -> List a -> List a
insert x Emp      = x ::: Emp
insert x (y:::ys)
  | x <= y        = x ::: y ::: ys
  | otherwise     = y ::: insert x ys
\end{spec}


<br>



Goal: Verified Insertion Sort
-----------------------------

<br>

**Goal:** specify & verify that output:

<br>

Is the same **size** as the input,

Has the same **elements** as the input,

Is in increasing **order**.



Property 1: Size
----------------

<br>



Exercise: `insert`
------------------

<br>

**Q:** Can you fix the type of `insert` so `sort` checks?

\begin{code}
{-@ sort :: (Ord a) => xs:List a -> ListN a {length xs} @-}
sort Emp          = Emp
sort (x:::xs)     = insert x (sort xs)

{-@ insert :: (Ord a) => a -> xs:List a -> List a @-}
insert x Emp      = x ::: Emp
insert x (y:::ys)
  | x <= y        = x ::: y ::: ys
  | otherwise     = y ::: insert x ys
\end{code}

<br>



Property 2: Elements
--------------------


<br>



Permutation
-----------

<br>

Same size is all fine, how about **same elements** in output?

<br>


SMT Solvers Reason About Sets
-----------------------------

<div class="fragment">

<br>

Hence, we can write *Set-valued* measures

<br>

Using the `Data.Set` API for convenience

<br>

\begin{spec} <div/>
import qualified Data.Set as S
\end{spec}

</div>

<br>

Specifying A `List`s Elements
-----------------------------

<br>

\begin{code}
{-@ measure elems @-}
elems :: (Ord a) => List a -> S.Set a
elems Emp      = S.empty
elems (x:::xs) = addElem x xs

{-@ inline addElem @-}
addElem :: (Ord a) => a -> List a -> S.Set a
addElem x xs = S.union (S.singleton x) (elems xs)
\end{code}

<div class="fragment">
`inline` lets us reuse Haskell terms in refinements.
</div>


Exercise: Verifying Permutation
-------------------------------

Lets verify that `sortE` returns the same set of elements:

\begin{code}
{-@ type ListE a S = {v:List a | elems v = S} @-}

{-@ sortE :: (Ord a) => xs:List a -> ListE a {elems xs} @-}
sortE Emp         = Emp
sortE (x:::xs)    = insertE x (sortE xs)

{-@ insertE :: (Ord a) => x:a -> xs:List a -> List a @-}
insertE x Emp     = x ::: Emp
insertE x (y:::ys)
  | x <= y        = x ::: y ::: ys
  | otherwise     = y ::: insertE x ys
\end{code}

**Q:** Can you fix the type for `insertE` so `sortE` verifies?

Property 3: Order
-----------------

<br>

Yes, yes, but does `sort` actually **sort** ?

<br>

<div class="fragment">

How to specify **ordered lists** ?

</div>

<br>

Recall: Refined Data Types
--------------------------

<br>



Refined Data: Ordered Pairs
---------------------------

<br>

Lets write a type for **ordered pairs**

\begin{code}
data OrdPair = OP {opX :: Int, opY :: Int}
\end{code}

<br>

<div class="fragment">
**Legal Values** value of `opX < opY`

\begin{spec}
okPair  = OP 2 4  -- legal
badPair = OP 4 2  -- illegal
\end{spec}
</div>


Exercise: Ordered Pairs
-----------------------

<br>

**Q:** Can you refine the data type to *legal* values only?

<br>

\begin{code}
{-@ data OrdPair = OP { opX :: Int, opY :: Int} @-}

okPair  = OP 2 4  -- legal
badPair = OP 4 2  -- illegal
\end{code}

<br>

Refined Data: CSV Tables
------------------------

<br>

\begin{code}
data Csv = Csv {
   hdrs :: List String
 , vals :: List (List Int)
 }

scores  = Csv {
   hdrs =  "Id" ::: "Midterm" ::: "Final" ::: Emp
 , vals = (   1 :::       25  :::      88 ::: Emp)
      ::: (   2 :::       27  :::      83 ::: Emp)
      ::: (   3 :::       19  :::      93 ::: Emp)
      ::: Emp
 }
\end{code}



Exercise: Valid CSV Tables
--------------------------

**Q:** Can you refine `Csv` so `scores'` is rejected?

\begin{code}
{-@ data Csv = Csv {
      hdrs :: List String
    , vals :: List (List Int)
    }                                          @-}

scores' = Csv {
   hdrs =  "Id" ::: "Midterm" ::: "Final" ::: Emp
 , vals = (   1 :::       25  :::      88 ::: Emp)
      ::: (   2 :::                    83 ::: Emp)
      ::: (   3 :::       19  :::      93 ::: Emp)
      ::: Emp
 }
\end{code}

Property 3: Ordered Lists
-------------------------

<br>

**Refine** the `List` data type to enforce **ordering**!

<br>


Lists
-----

<br>

Lets **define** a type for ordered lists

<br>

\begin{code}
data OList a =
      OEmp
    | (:<:) { oHd :: a
            , oTl :: OList a }
\end{code}

<br>


Ordered Lists
-------------

<br>

Lets **refine** the type to enforce **order**

\begin{code}
{-@ data OList a =
      OEmp
    | (:<:) { oHd :: a
            , oTl :: OList {v:a | oHd <= v}} @-}
\end{code}

<br>

Head `oHd` is **smaller than every value** `v` in tail `oTl`


Ordered Lists
-------------

<br>

*Illegal values unrepresentable*

<br>

\begin{code}
okList :: OList Int
okList = 1 :<: 2 :<: 3 :<: OEmp

badList :: OList Int
badList = 1 :<: 3 :<: 2 :<: OEmp
\end{code}


Exercise: Insertion Sort
------------------------

<br>

**Q:** Oops. There's a problem! Can you fix it?

\begin{code}
{-@ sortO ::  xs:List a -> OListE a {elems xs} @-}
sortO Emp      = OEmp
sortO (x:::xs) = insertO x (sortO xs)

{-@ insertO :: x:a -> xs:_  -> OListE a {addElemO x xs} @-}
insertO x (y :<: ys)
  | x <= y     = y :<: x :<: ys
  | otherwise  = y :<: insertO x ys
insertO x _    = x :<: OEmp
\end{code}


Multiple Measures
-----------------


Different Measures for `List`
-----------------------------

<br>

We just wrote *two* measures for `List`

<br>

+ `length :: List a -> Nat`
+ `elems  :: List a -> Set a`


Multiple Measures are Conjoined
-------------------------------

<br>

Data constructor refinements are **conjoined**

\begin{spec}
data List a where
  Emp   :: {v:List a |  length v = 0
                     && elems v  = empty}
  (:::) :: x:a
        -> xs:List a
        -> {v:List a |  length v = 1 + length xs
                     && elems v  = addElem x  xs }
\end{spec}

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

Continue
--------

<br>

**Other Case Studies**

+ [Well Scoped Evaluator](http://ucsd-progsys.github.io/lh-workshop/05-case-study-eval.html)
+ [Low-level Memory](http://ucsd-progsys.github.io/lh-workshop/06-case-study-bytestring.html)

<br>

**Continue:** [[Part II : Proofs]](05-termination.html)
