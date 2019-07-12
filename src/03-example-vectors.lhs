<div class="hidden">
\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}

module Examples
       ( sum
       , sum'
       , sum''
       , range
       -- , average
       -- , length
       )
       where

import Prelude hiding (map, sum, length, (!))

data Vector a

at   :: Vector a -> Int -> a
at = undefined

size :: Vector a -> Int
size = undefined

-- {-@ range :: lo:_ -> hi:{lo <= hi} -> [{v:_ | lo <= v && v < hi}] @-}
\end{code}
</div>

Case Study: Vector Bounds
-------------------------

<br> 

**Goal: Whirlwind Overview** 

<div class="mybreak"><br></div>

Specifications

Verification

Inference

Collections & HOFs

Case Study: Vector Bounds
-------------------------

<br> 

**Goal: Whirlwind Overview** 

<div class="mybreak"><br></div>

<font color="#1569C7">Specifications</font>

Verification

Inference

Collections & HOFs



Case Study: Vector Bounds
-------------------------

<br> 

**Goal: Whirlwind Overview** 

<div class="mybreak"><br></div>

Specifications

<div class="mybreak"><br></div>

**Property: In-bounds Array Access**

Case Study: Vector Bounds
-------------------------

<br> 

**Goal: Whirlwind Overview** 

<div class="mybreak"><br></div>

Specifications

<div class="mybreak"><br></div>

**Property: In-bounds Array Access**

\begin{code}
{-@ measure vlen :: Vector a -> Int @-}
\end{code}

An **uninterpreted function** describing the **size** of a `Vector`

Specifications: Pre-Conditions
------------------------------

<div class="mybreak"><br></div>

What does a function **require** for correct execution?



Specifications: Pre-Conditions
------------------------------

<div class="mybreak"><br></div>

What does a function **require** for correct execution?

<div class="mybreak"><br></div>

\begin{code}
{-@ at :: vec:Vector a -> {i:Nat| i < vlen vec} -> a @-}
\end{code}

<div class="mybreak"><br></div>

**Refinement on the function's Input Type**

Input index must be between `0` and the size of `vec`

Specifications: Post-Conditions
-------------------------------

<div class="mybreak"><br></div>

What does a function **ensure** about its result?


Specifications: Post-Conditions
-------------------------------

<div class="mybreak"><br></div>

What does a function **ensure** about its result?

<div class="mybreak"><br></div>

\begin{code}
{-@ size :: vec:Vector a -> {n:Nat | n == vlen vec} @-}
\end{code}

<div class="mybreak"><br></div>

**Refinement on the function's Output Type**

Returned value equals the size of the input `vec` 


Case Study: Vector Bounds
-------------------------

<br> 

**Goal: Whirlwind Overview** 

<div class="mybreak"><br></div>

Specification

<font color="#1569C7">Verification</font>

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

**Exercise:** Does the above verify? If not, can you fix it so it does?

Verification: Vector Sum
------------------------

<img src="img/sum-code-numbers.png" height=200px>

Verification: Vector Sum
------------------------

<img src="img/sum-code-numbers.png" height=200px>

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

<br> 

**Goal: Whirlwind Overview** 

<div class="mybreak"><br></div>

Specification

Verification

<font color="#1569C7">Inference</font>

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

<img src="img/sum-code-infer.png" height=200px>


Inference: Vector Sum
---------------------

<img src="img/sum-code-infer.png" height=200px>

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

<br> 

**Goal: Whirlwind Overview** 

<div class="mybreak"><br></div>

Specification

Verification

Inference

<font color="#1569C7">Collections & HOFs</font>


Collections & Higher-Order Functions
------------------------------------

<div class="mybreak"><br></div>

**HOFs >> Recursion!**

Collections & Higher-Order Functions
------------------------------------

<div class="mybreak"><br></div>

**HOFs >> Recursion!**

<br>

[Example: AWS Pagination API (OLD)](https://docs.aws.amazon.com/sdk-for-java/v2/developer-guide/examples-pagination.html)


Collections & Higher-Order Functions
------------------------------------

<div class="mybreak"><br></div>

**HOFs >> Recursion!**

<div class="mybreak"><br></div>

**Example: AWS Pagination API (OLD)**

<p align=center>
<img src="img/aws-pag-while.png" height=220px style="-webkit-filter: drop-shadow(5px 5px 5px #222); filter: drop-shadow(5px 5px 5px #222);"/>
</p>

Collections & Higher-Order Functions
------------------------------------

<div class="mybreak"><br></div>

**HOFs >> Recursion!**

<div class="mybreak"><br></div>

**Example: AWS Pagination API (NEW)**

<p align=center>
<img src="img/aws-pag-stream.png" height=40px style="-webkit-filter: drop-shadow(5px 5px 5px #222); filter: drop-shadow(5px 5px 5px #222);"/>
</p>


Collections & Higher-Order Functions
------------------------------------

<div class="mybreak"><br></div>

**Refining Sequences**

\begin{code}
range :: Int -> Int -> [Int]
range lo hi
  | lo < hi   = lo : range (lo + 1) hi
  | otherwise = []
\end{code}

<div class="mybreak"><br></div>

**Exercise:** Can you write down a good type for `range`?





Collections & Higher-Order Functions
------------------------------------

<div class="mybreak"><br></div>

**Reduce over Sequences (c.f. Map-Reduce)**

\begin{code}
reduce :: (a -> b -> a) -> a -> [b] -> a
reduce f acc []     = acc 
reduce f acc (x:xs) = let acc' = f acc x 
                      in
                          reduce f acc' xs
\end{code}

**Type of `reduce` looks like Floyd-Hoare rule for Loops!**

Type `a` is an *invariant* that holds on *initial* `acc` and is *inductively* by `f`

Collections & Higher-Order Functions
------------------------------------

<div class="mybreak"><br></div>

**Vector Sum by Reduction**

\begin{code}
sum''   :: Vector Int -> Int
sum'' vec = let is  = range 0 (size vec)
                add = \n i -> n + at vec i 
            in 
              reduce add 0 is               
\end{code}

Polymorphic types make refinement inference "just work" ...

\begin{spec}<div/>
     is  :: [{i:|0 <= i < len vec}]              
     add :: Int  -> {i:|0 <= i < len vec} -> Int 
\end{spec}

[Bonus slides: Constraints and Solution for `range`](03-bonus.html)


Refinement Types and Collections
--------------------------------

<div class="mybreak"><br></div>

**Types Simplify Checking Properties of Collections** 

Type system = Algorithm for *generalization* and *instantiation* 

<div class="mybreak"><br></div>

Refinement Types and Collections
--------------------------------

<div class="mybreak"><br></div>

**Types Simplify Checking Properties of Collections** 

Type system = Algorithm for *generalization* and *instantiation* 

**Generalization**

Lift properties from single _value_ to whole _collection_

e.g. from individual values to whole collection in `range`

<div class="mybreak"><br></div>

Refinement Types and Collections
--------------------------------

<div class="mybreak"><br></div>

**Types Simplify Checking Properties of Collections** 

Type system = Algorithm for *generalization* and *instantiation* 

**Generalization**

Lift properties from single _value_ to whole _collection_

e.g. from individual values to whole collection in `range`

<div class="mybreak"><br></div>

**Instantiation**

Apply properties from whole _collection_ to single _value_ 

e.g. from whole collection to individual index in `sum`


Case Study: Vector Bounds
-------------------------

<br> 

**Recap: Whirlwind Overview** 

<div class="mybreak"><br></div>

Specifications

Verification

Inference

Collections & HOFs

Plan
----

<br>
<br>

**Part I:** [Refinements 101](02-refinements.html)

Case Study: [Vector Bounds](03-example-vectors.html)

<br>

**Part II:** **[Properties of Structures](04-data-properties.html)**

Case Study: [Sorting](05-example-sort.html), [Interpreter](06-example-interpreter.html)

Bonus Slides: Verifying `range`
-------------------------------

\begin{spec}<div/>
range :: lo:_ -> hi:{lo <= hi} -> [{v:_ | lo <= v && v < hi}] 
range lo hi
  | lo < hi   = let tail = range (lo + 1) hi  ---------- (1)
                in  lo : tail -------------------------- (2-lo, 2-tail, 2-res)
  | otherwise = [] ------------------------------------- (3)  
\end{spec}

**Polymorphic Type for "Cons"**

\begin{spec}<div/>
    (:) :: a -> [a] -> [a]
\end{spec}

**Polymorphic Instance for "Cons"** 

\begin{spec}<div/>
    (:) :: {v:Int|K(v)} -> [{v:Int|K(v)}] -> [{v:Int|K(v)}]
\end{spec}

where `a` replaced with unknown `{v:Int|K(v)}`


Subtyping Constraints
---------------------

\begin{spec}<div/>
range :: lo:_ -> hi:{lo <= hi} -> [{v:_ | lo <= v && v < hi}] 
range lo hi
  | lo < hi   = let tail = range (lo + 1) hi  ---------- (1)
                in  lo : tail -------------------------- (2-lo, 2-tail, 2-res)
  | otherwise = [] ------------------------------------- (3)  
\end{spec}

Yields the **Subtyping Constraints**

\begin{spec}<div/>
lo < hi |- {v:Int|v = lo+1}     <: {v:Int|v <= hi}    -- (1)
lo < hi |- {v:Int|v = lo}       <: {v:Int|K(v)}       -- (2-lo)
lo < hi |- [{v:Int|lo+1<=v<hi}] <: [{v:Int|K(v)}]     -- (2-tail)
lo < hi |- [{v:Int|K(v)}]       <: [{v:Int|lo<=v<hi}] -- (2-res)
lo >=hi |- [{v:Int|false}]      <: [{v:Int|lo<=v<hi}] -- (3)
\end{spec}

Horn Constraints
----------------

\begin{spec}<div/>
range :: lo:_ -> hi:{lo <= hi} -> [{v:_ | lo <= v && v < hi}] 
range lo hi
  | lo < hi   = let tail = range (lo + 1) hi  ---------- (1)
                in  lo : tail -------------------------- (2-lo, 2-tail, 2-res)
  | otherwise = [] ------------------------------------- (3)  
\end{spec}

**List Subtyping reduces to element-wise Subtyping**

\begin{spec}<div/>
lo < hi => (v = lo+1)   => (v <= hi)                  -- (1)
lo < hi => (v = lo)     => K(v)                       -- (2-lo)
lo < hi => (lo+1<=v<hi) => K(v)                       -- (2-tail)
lo < hi => K(v)         => (lo <= v < hi)             -- (3)
lo >=hi => false        => (lo <= v < hi)             -- (3)
\end{spec}

**Solution**

\begin{spec}<div/>
K(v) := lo <= v < hi
\end{spec}



[pldi08]: http://dl.acm.org/citation.cfm?id=1375602