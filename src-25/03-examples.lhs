<div class="hidden">
\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}

module Examples
       ( sum
       , sum'
       , sum''
       , average
       )
       where

import Prelude hiding (foldr, map, sum, length, (!))

data Vector a

range :: Int -> Int -> [Int]

at   :: Vector a -> Int -> a
at = undefined

size :: Vector a -> Int
size = undefined
\end{code}
</div>

Refinement Types by Example
---------------------------

<div class="mybreak"><br></div>

Specifications

Verification

Inference

Collections & HOFs

Refinements for Datatypes

Refinement Types by Example
---------------------------

<div class="mybreak"><br></div>

**Specifications**

Verification

Inference

Collections & HOFs

Refinements for Datatypes


Refinement Types by Example
---------------------------

<div class="mybreak"><br></div>

Specifications

<div class="mybreak"><br></div>

**Property: In-bounds Array Access**

Refinement Types by Example
---------------------------

<div class="mybreak"><br></div>

Specifications

<div class="mybreak"><br></div>

**Property: In-bounds Array Access**

\begin{code}
{-@ measure vlen :: Vector a -> Int @-}
\end{code}

Specifications: Pre-Conditions
------------------------------

<div class="mybreak"><br></div>

What does a function **require** for correct execution?

<div class="mybreak"><br></div>

\begin{code}
{-@ at :: v:Vector a -> {i:Nat| i < vlen v} -> a @-}
\end{code}

<div class="mybreak"><br></div>

Refinement on the function's **input type**


Specifications: Post-Conditions
-------------------------------

<div class="mybreak"><br></div>

What does a function **ensure** about its result?

<div class="mybreak"><br></div>

\begin{code}
{-@ size :: v:Vector a -> {n:Int | n == vlen v} @-}
\end{code}

<div class="mybreak"><br></div>

Refinement on the function's **output type**

Refinement Types by Example
---------------------------

<div class="mybreak"><br></div>

Specifications

**Verification**

Inference

Collections & HOFs

Refinements for Datatypes

Verification: Vector Sum
------------------------

<div class="mybreak"><br></div>

\begin{code}
sum :: Vector Int -> Int
sum v = loop 0
  where
    {-@ loop :: Nat -> Int @-}
    loop i
      | i <= size v = at v i + loop (i + 1)
      | otherwise   = 0
\end{code}

<div class="mybreak"><br></div>

Oops! What gives?

Verification: Vector Sum
------------------------

<img src="img/sum-code-numbers.png" height=150px>

Verification: Vector Sum
------------------------

<img src="img/sum-code-numbers.png" height=150px>

**Verification Conditions**

$$\begin{array}{lll}
\True
  & \Rightarrow v = 0
  & \Rightarrow 0 \leq v
  & \mbox{(A)} \\
0 \leq i \wedge n = \mathit{vlen}\ v \wedge i < n
  & \Rightarrow v = i + 1
  & \Rightarrow 0 \leq v
  & \mbox{(B)} \\
0 \leq i \wedge n = \mathit{vlen}\ v \wedge i < n
  & \Rightarrow v = i
  & \Rightarrow 0 \leq v < \mathit{vlen}\ v
  & \mbox{(C)} \\
\end{array}$$


Refinement Types by Example
---------------------------

<div class="mybreak"><br></div>

Specifications

Verification

**Inference**

Collections & HOFs

Refinements for Datatypes

Inference
---------

<br>

    The more interesting your types get,
    the less fun it is to write them down.
                       -- Benjamin Pierce

Inference: Vector Sum
----------------------

<div class="mybreak"><br></div>

\begin{code}
sum' :: Vector Int -> Int
sum' v = loop 0
  where
    {-@ loop :: _ -> _ @-}
    loop i
      | i < size v = at v i + loop (i + 1)
      | otherwise  = 0
\end{code}


Inference: Vector Sum
---------------------

<br>

Not magic, just **Abstract Interpretation**

Inference: Vector Sum
---------------------

<br>

Not magic, just Abstract Interpretation

Represent **unknown refinements** with $\kvar{}{\cdot}$ variables ...

... Solve resulting **Horn Constraints**

<br>

[[PLDI 2008]][pldi08]

Inference: Vector Sum
---------------------

<img src="img/sum-code-infer.png" height=150px>


Inference: Vector Sum
---------------------

<img src="img/sum-code-infer.png" height=150px>

**Horn Constraints**

$$\begin{array}{lll}
\True
  & \Rightarrow v = 0
  & \Rightarrow \kvar{}{v}
  & \mbox{(A)} \\
\kvar{}{i} \wedge n = \mathit{vlen}\ v \wedge i < n
  & \Rightarrow v = i + 1
  & \Rightarrow \kvar{}{v}
  & \mbox{(B)} \\
\kvar{}{i} \wedge n = \mathit{vlen}\ v \wedge i < n
  & \Rightarrow v = i
  & \Rightarrow 0 \leq v < \mathit{vlen}\ v
  & \mbox{(C)} \\
\end{array}$$

Inference: Vector Sum
---------------------

<div class="mybreak"><br></div>

**Horn Constraints**

$$\begin{array}{lll}
\True
  & \Rightarrow v = 0
  & \Rightarrow \kvar{}{v}
  & \mbox{(A)} \\
\kvar{}{i} \wedge n = \mathit{vlen}\ v \wedge i < n
  & \Rightarrow v = i + 1
  & \Rightarrow \kvar{}{v}
  & \mbox{(B)} \\
\kvar{}{i} \wedge n = \mathit{vlen}\ v \wedge i < n
  & \Rightarrow v = i
  & \Rightarrow 0 \leq v < \mathit{vlen}\ v
  & \mbox{(C)} \\
\end{array}$$

<div class="mybreak"><br></div>

**Synthesized Solution**

$$\kvar{}{v} = 0 \leq v$$

Refinement Types by Example
---------------------------

<div class="mybreak"><br></div>

Specifications

Verification

Inference

**Collections & HOFs**

Refinements for Datatypes


Collections & Higher-Order Functions
------------------------------------

<div class="mybreak"><br></div>

**Composition >> Recursion!**

Collections & Higher-Order Functions
------------------------------------

<div class="mybreak"><br></div>

**Generic Sequences**

\begin{code}
range lo hi
  | lo < hi   = lo : range (lo + 1) hi
  | otherwise = []
\end{code}

<div class="mybreak"><br></div>

(What's a good type for `range`?)


Collections & Higher-Order Functions
------------------------------------

<div class="mybreak"><br></div>

**Fold over Sequences**

\begin{code}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b []     = b
foldr f b (x:xs) = f x (foldr f b xs)
\end{code}

Collections & Higher-Order Functions
----------------------

<div class="mybreak"><br></div>

**"Wholemeal" Vector Sum**

\begin{code}
sum''   :: Vector Int -> Int
sum'' v = foldr add 0 is
  where
    add = \i n -> n + at v i
    is  = range 0 (size v)
\end{code}

Types make refinement inference *"just work"* ...


Refinement Types by Example
---------------------------

<div class="mybreak"><br></div>

Specifications

Verification

Inference

Collections & HOFs

**Refinements for Datatypes**

Example: List `average`
-----------------------

\begin{code}
{-@ average :: [Int] -> Int @-}
average xs  = total `div` n
  where
    total   = foldr (+) 0 xs
    n       = length xs

length        :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs
\end{code}

Yikes! `average` requires **non-empty** lists!

Refinements for Datatypes
-------------------------

**Lift** (some) functions into specification logic:

\begin{code}
{-@ measure length @-}
\end{code}

which lets us define a type alias

\begin{code}
{-@ type ListNE a = {v:[a] | 0 < length v} @-}
\end{code}

<div class="mybreak"><br></div>

Now lets go back and *fix* `average` ...

Measures Yield Refined Constructors
-----------------------------------

**Lift** (some) functions into specification logic:

\begin{spec}
data [a] where
  []  :: {v:[a] | length v = 0}
  (:) :: a
      -> t:[a]
      -> {v:[a] | length v = 1 + length t}
\end{spec}

Where `length` is **uninterpreted** in refinement Logic

Example: `map` over Lists
-------------------------

What's the problem here? (Lets fix it!)

\begin{code}
{-@ hwAverage :: ListNE (a, Int) -> Int @-}
hwAverage nxs = average (map snd nxs)

{-@ map :: (a -> b) -> [a] -> [b] @-}
map f []     = []
map f (x:xs) = f x : map f xs
\end{code}

Refinements for Datatypes
-------------------------

<div class="mybreak"><br></div>

**Measures**

Specify properties as *functions over datatypes*


Refinements for Datatypes
-------------------------

<div class="mybreak"><br></div>

**Measures**

Specify properties as *functions over datatypes*

<div class="mybreak"><br></div>

**Refined Constructors**

Instantiate constraints at *fold* (`C ...`) & *unfold* (`case-of`)


Refinements for Datatypes
-------------------------

<div class="mybreak"><br></div>

**Measures**

Specify properties as functions over datatypes

<div class="mybreak"><br></div>

**Refined Constructors**

Instantiate constraints at *fold* (`C ...`) & *unfold* (`case-of`)

<div class="mybreak"><br></div>

**Automate verification of data types**

Refinement Types by Example
---------------------------

<div class="mybreak"><br></div>

Specifications

Verification

Inference

Collections & HOFs

Refinements for Datatypes

[[continue]](04-abstracting.html)

[pldi08]: http://dl.acm.org/citation.cfm?id=1375602
