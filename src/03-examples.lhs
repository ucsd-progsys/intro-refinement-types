<div class="hidden">
\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}

module IntroToRefinementTypes
       ( sum
       -- , sumInfer
       -- , sumHO
       -- , average
       -- , insertSort
       )
       where

import Prelude hiding (foldr, map, sum, length, (!))

data Vector a


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

Invariants & Datatypes

Refinement Types by Example
---------------------------

<div class="mybreak"><br></div>

**Specifications**

Verification

Inference

Collections & HOFs

Invariants & Datatypes


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

Invariants & Datatypes

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

\begin{spec}
sum :: Vector Int -> Int
sum v = loop 0
  where
    {-@ loop :: Nat -> Int @-}
    loop i
      | i <= size v = at v i + loop (i + 1)
      | otherwise   = 0
\end{spec}
