<div class="hidden">
\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}

module Examples
       ( sum
       , sum'
       , sum''
       , average
       , length
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

Case Study: Vector Bounds
-------------------------

<div class="mybreak"><br></div>

Specifications

Verification

Inference

Collections & HOFs

Case Study: Vector Bounds
-------------------------

<div class="mybreak"><br></div>

**Specifications**

Verification

Inference

Collections & HOFs

Refinements for Datatypes


Case Study: Vector Bounds
-------------------------

<div class="mybreak"><br></div>

Specifications

<div class="mybreak"><br></div>

**Property: In-bounds Array Access**

Case Study: Vector Bounds
-------------------------

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
{-@ at :: vec:Vector a -> {i:Nat| i < vlen vec} -> a @-}
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

Case Study: Vector Bounds
-------------------------

<div class="mybreak"><br></div>

Specifications

**Verification**

Inference

Collections & HOFs

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


Case Study: Vector Bounds
---------------------------

<div class="mybreak"><br></div>

Specifications

Verification

**Inference**

Collections & HOFs

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

Case Study: Vector Bounds
---------------------------

<div class="mybreak"><br></div>

Specifications

Verification

Inference

**Collections & HOFs**


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


Case Study: Vector Bounds
---------------------------

<div class="mybreak"><br></div>

Specifications

Verification

Inference

Collections & HOFs

Case Study: Vector Bounds
---------------------------

<div class="mybreak"><br></div>

Specifications

Verification

Inference

Collections & HOFs

Refinements for Datatypes

Plan
----

<br>
<br>

**Part I:** [Refinements 101](02-refinements.html)

Case Study: [Vector Bounds](03-example-vectors.html)

<br>

**Part II:** **[Properties of Structures](04-data-properties.html)**

Case Study: [MergeSort](05-example-mergesort.html), [Interpreter](06-example-interpreter.html)


[pldi08]: http://dl.acm.org/citation.cfm?id=1375602
