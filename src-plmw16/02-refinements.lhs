<div class="hidden">

\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--diff"           @-}
{-@ LIQUID "--short-names"    @-}

module Refinements where
import Prelude hiding (abs, max)

nats, poss  :: [Int]
zero        :: Int
zero'       :: Int
four        :: Int
safeDiv     :: Int -> Int -> Int
size, size' :: [a] -> Int
\end{code}

</div>


Simple Refinement Types
-----------------------

<br>

Refinement Types = **Types** + **Predicates**


Types
-----

\begin{spec}<div/>
b := Int | Bool | ...  -- primitives
   | a, b, c           -- type variables
\end{spec}

\begin{spec}<div/>
t := {x:b | p}         -- refined base
   | x:t -> t          -- refined function
\end{spec}

`p` is a predicate from a **decidable logic**

Predicates
----------

\begin{spec} <div/>
p := e         -- atom
   | e1 == e2  -- equality
   | e1 <  e2  -- ordering
   | p && p    -- and
   | p || p    -- or
   | not p     -- negation
\end{spec}

\begin{spec} <div/>
e := x, y, z,...  -- variable
   | 0, 1, 2,...  -- constant
   | e + e        -- addition
   | e - e        -- subtraction
   | c * e        -- linear multiplication
   | f e1 ... en  -- uninterpreted function
\end{spec}

Predicates
----------

\begin{spec} <div/>
p := e           -- atom
   | e1 == e2    -- equality
   | e1 <  e2    -- ordering
   | (p && p)    -- and
   | (p || p)    -- or
   | (not p)     -- negation
\end{spec}

<br>

Expressions
-----------

<br>

\begin{spec} <div/>
e := x, y, z,...    -- variable
   | 0, 1, 2,...    -- constant
   | (e + e)        -- addition
   | (e - e)        -- subtraction
   | (c * e)        -- linear multiplication
   | (f e1 ... en)  -- uninterpreted function
\end{spec}

<div class="fragment">

**Refinement Logic: QF-UFLIA**

Quantifier-Free Logic of Uninterpreted Functions and Linear Arithmetic

</div>


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Example: Natural Numbers
------------------------

<br>

\begin{code}
{-@ type Nat = {v:Int | 0 <= v} @-}

{-@ zero :: Nat   @-}
zero     =  0

{-@ nats :: [Nat] @-}
nats     =  [0, 1, 2, 3]
\end{code}

<br>


<div class="fragment">
Refinement types via special comments `{-@ ... @-}`
</div>


Example: Natural Numbers
------------------------

<br>

\begin{code}
{-@ type Nat = {v:Int | 0 <= v} @-}

{-@ nats :: [Nat]               @-}
nats     =  [0, 1, 2, 3]
\end{code}

<br>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Exercise: Positive Integers
---------------------------

<br>

\begin{code}
{-@ type Pos = {v:Int | 0 <= v} @-}

{-@ poss :: [Pos]               @-}
poss     =  [0, 1, 2, 3]
\end{code}

<br>

**Q:** First, can you fix `Pos` so `poss` is **rejected**?

<br>

<div class="fragment">
**Q:** Next, can you modify `poss` so it is **accepted**?
</div>


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Refinement Type Checking
========================


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>






A Term Can Have *Many* Types
----------------------------

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


A Term Can Have *Many* Types
----------------------------

<br>

<div class="fragment">
What *is* the type of `0` ?

<br>

\begin{spec}
{-@ zero  :: Zero @-}
zero      = 0

{-@ zero' :: Nat  @-}
zero'     = zero
\end{spec}

</div>

Predicate Subtyping [[NUPRL, PVS]](http://pvs.csl.sri.com/papers/subtypes98/tse98.pdf)
----------------------------------

<br>

In **environment** $\Gamma$ the type $t_1$ is a **subtype** of the type $t_2$

<br>

$$\boxed{\Gamma \vdash t_1 \preceq t_2}$$

<div class="fragment">

<br>

**Environment** $\Gamma$ is a sequence of binders

<br>

$$\Gamma \doteq \overline{\bindx{x_i}{P_i}}$$

</div>


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Predicate Subtyping [[NUPRL, PVS]](http://pvs.csl.sri.com/papers/subtypes98/tse98.pdf)
--------------------

<br>

$$\boxed{\Gamma \vdash t_1 \preceq t_2}$$


<br>

$$
\begin{array}{rll}
{\mathbf{If\ VC\ is\ Valid}}   & \bigwedge_i P_i \Rightarrow  Q  \Rightarrow R & (\mbox{By SMT}) \\
                &  & \\
{\mathbf{Then}} & \overline{\bindx{x_i}{P_i}} \vdash \reft{v}{b}{Q} \preceq \reft{v}{b}{R} & \\
\end{array}
$$


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Example: Natural Numbers
------------------------

<br>

<div class="fragment">

$$
\begin{array}{rcrccll}
\mathbf{VC\ is\ Valid:} & \True     & \Rightarrow &  v = 0   & \Rightarrow &  0 \leq v & \mbox{(by SMT)} \\
                        &           &             &          &             &           &        \\
\mathbf{So:}            & \emptyset & \vdash      & \Zero    & \preceq     & \Nat      &   \\
\end{array}
$$
</div>

<br>

<div class="fragment">

And so, we can type:

\begin{code}
{-@ zero' :: Nat @-}
zero'     =  zero   -- zero :: Zero <: Nat
\end{code}
</div>


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>



Example: Natural Numbers
------------------------

<br>

<div class="fragment">

$$
\begin{array}{rcrccll}
\mathbf{VC\ is\ Valid:} & x = 3 & \Rightarrow &  v = x + 1 & \Rightarrow &  0 \leq v & \mbox{(by SMT)} \\
                        &       &             &            &             &               \\
\mathbf{So:}            & x = 3 & \vdash      & \Zero      & \preceq     & \Nat      &   \\
\end{array}
$$
</div>

<br>

<div class="fragment">

And so, we can type:

\begin{code}
{-@ four :: Nat @-}
four  = x + 1          -- x = 3 |- {v = x + 1} <: Nat
  where
    x = 3
\end{code}
</div>


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>





[SMT](http://en.wikipedia.org/wiki/Satisfiability_modulo_theories) Automates Subtyping
------------------------

<br>

**Eliminates boring proofs** ... makes verification practical.

<br>


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Contracts: Function Types
=========================

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>




Pre-Conditions
--------------


<br>

\begin{code}
{-@ impossible :: {v:_ | false} -> a @-}
impossible msg = error msg
\end{code}

<br>

<div class="fragment">
No value satisfies `false` $\Rightarrow$ **no valid inputs** for `impossible`
</div>

<br>

<div class="fragment">
Program type-checks $\Rightarrow$ `impossible` **never called at run-time**
</div>


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Exercise: Pre-Conditions
------------------------

<br>

Let's write a **safe division** function

<br>

\begin{code}
{-@ safeDiv :: Int -> Int -> Int   @-}
safeDiv _ 0 = impossible "divide-by-zero"
safeDiv x n = x `div` n
\end{code}

<br>

<div class="fragment">
**Q:** Yikes! Can you **fix the type** of `safeDiv` to banish the error?
</div>


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Precondition Checked at Call-Site
---------------------------------

<br>

\begin{code}
avg2 x y   = safeDiv (x + y) 2
\end{code}

<div class="fragment">
**Precondition**

\begin{spec} <div/>
{-@ safeDiv :: n:Int -> d:NonZero -> Int @-}
\end{spec}
</div>

<br>

**Verifies As**

$$\inferrule{}{(v = 2) \Rightarrow (v \not = 0)}
            {\bindx{x}{\Int}, \bindx{y}{\Int} \vdash \reftx{v}{v = 2} \preceq \reftx{v}{v \not = 0}}
$$


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Exercise: Check That Data
-------------------------

<br>

\begin{code}
calc :: IO ()
calc = do
  putStrLn "Enter numerator"
  n <- readLn
  putStrLn "Enter denominator"
  d <- readLn
  putStrLn ("Result = " ++ show (safeDiv n d))
  calc
\end{code}

<br>

**Q:** Can you fix `calc` so it typechecks?


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Precondition Checked at Call-Site
---------------------------------

<br>

\begin{code}
avg        :: [Int] -> Int
avg xs     = safeDiv total n
  where
    total  = sum    xs
    n      = length xs         -- returns a Nat
\end{code}

<br>

<div class="fragment">
**Rejected** as `n` can be *any* `Nat`

$$0 \leq n \Rightarrow (v = n) \not \Rightarrow (v \not = 0)$$

</div>


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

`size` returns positive values
------------------------------

<br>

Specify **post-condition** as **output type**

<br>

\begin{code}
{-@ size :: [a] -> Pos @-}
size [_]    = 1
size (_:xs) = 1 + size xs
-- size _   = impossible "size"
\end{code}


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Postconditions Checked at Return
--------------------------------

<br>

\begin{spec} <div/>
{-@ size    :: [a] -> Pos @-}
size []     = 1                        -- (1)
size (_:xs) = 1 + n  where n = size xs -- (2)
\end{spec}

<br>

<div class="fragment">
**Verified As**

$$\begin{array}{rll}
\True   & \Rightarrow (v = 1)     & \Rightarrow (0 < v) & \qquad \mbox{at (1)} \\
(0 < n) & \Rightarrow (v = 1 + n) & \Rightarrow (0 < v) & \qquad \mbox{at (2)} \\
\end{array}$$
</div>


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Verifying `avg`
---------------

<br>

\begin{code}
avg' xs    = safeDiv total n
  where
    total  = sum  xs
    n      = size xs           -- returns a Pos
\end{code}

<br>

<div class="fragment">
**Verifies As**

$$(0 < n) \Rightarrow (v = n) \Rightarrow (v \not = 0)$$
</div>


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Recap
-----

<br>

<div class="fragment">
**Refinement Types**
Types + Predicates
</div>

<br>

<div class="fragment">
**Specify Properties**

Via Refined Input- and Output- Types
</div>

<br>

<div class="fragment">
**Verify Properties**

Via SMT based Predicate Subtyping
</div>


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


Unfinished Business
-------------------

<br>

How to prevent calling `size` with **empty lists**?

<br>

\begin{code}
{-@ size'    :: [a] -> Pos @-}
size' [_]    = 1
size' (_:xs) = 1 + size' xs
size' _      = impossible "size"
\end{code}

<br>

<div class="fragment">
Next: How to **describe properties of** structures [[continue...]](03-datatypes.html)
</div>


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
