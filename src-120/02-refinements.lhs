<div class="hidden">

\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--diff"           @-}
{-@ LIQUID "--short-names"    @-}

module SimpleRefinements where
import Prelude hiding (abs, max)

nats, poss  :: [Int]
zeroA, zeroB :: Int
zero        :: Int
zero'       :: Int
four        :: Int
safeDiv     :: Int -> Int -> Int
size, size' :: [a] -> Int
\end{code}

</div>


<br>
<br>
<br>
<br>


Simple Refinement Types
------------------

<br>

Refinement Types = *Types* + *Predicates*


Types
-----

<br>

\begin{spec}<div/>
b := Int         -- base types
   | Bool
   | ...
   | a, b, c     -- type variables

t := {x:b | p}   -- refined base
   | x:t -> t    -- refined function

p := ...         -- predicate in decidable logic
\end{spec}



Predicates
----------

<br>

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


Example: Integers equal to `0`
------------------------------

<br>

\begin{code}
{-@ type Zero = {v:Int | v = 0} @-}

{-@ zero :: Zero @-}
zero     =  0
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


Exercise: Positive Integers
---------------------------

<br>

\begin{code}
{-@ type Pos = {v:Int | 0 <= v} @-}

{-@ poss :: [Pos]               @-}
poss     =  [0, 1, 2, 3]
\end{code}

**Q:** First, can you fix `Pos` so `poss` is **rejected**?

**Q:** Next, can you modify `poss` so it is **accepted**?



Refinement Type Checking
------------------------


<br>


A Term Can Have *Many* Types
----------------------------

<br>


A Term Can Have *Many* Types
----------------------------


<br>

\begin{code}
{-@ zeroA :: Zero @-}
zeroA     = 0

{-@ zeroB :: Nat  @-}
zeroB     = 0
\end{code}



<br>

Predicate Subtyping [[NUPRL, PVS]](http://pvs.csl.sri.com/papers/subtypes98/tse98.pdf)
----------------------------------

<br>

In **environment** $\Gamma$ the type $t_1$ is a **subtype** of the type $t_2$


$$\boxed{\Gamma \vdash t_1 \preceq t_2}$$

<div class="fragment">

<br>

**Environment** $\Gamma$ is a sequence of binders


$$\Gamma \doteq \overline{\bindx{x_i}{P_i}}$$

</div>


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


<div class="fragment">

And so, we can type:

\begin{code}
{-@ zero' :: Nat @-}
zero'     =  zero   -- zero :: Zero <: Nat
\end{code}
</div>


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

<div class="fragment">

And so, we can type:

\begin{code}
{-@ four :: Nat @-}
four  = x + 1          -- x = 3 |- {v = x + 1} <: Nat
  where
    x = 3
\end{code}
</div>


[SMT](http://en.wikipedia.org/wiki/Satisfiability_modulo_theories) Automates Subtyping
------------------------

<br>

**Eliminates boring proofs** ... makes verification practical.

<br>


<br>


Contracts: Function Types
-------------------------

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

<div class="fragment">
Program type-checks $\Rightarrow$ `impossible` **never called at run-time**
</div>


<br>

Exercise: Pre-Conditions
------------------------

<br>

Let's write a **safe division** function

\begin{code}
{-@ safeDiv :: Int -> Int -> Int   @-}
safeDiv _ 0 = impossible "divide-by-zero"
safeDiv x n = x `div` n
\end{code}

<br>

<div class="fragment">
**Q:** Yikes! Can you **fix the type** of `safeDiv` to banish the error?
</div>


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

**Verifies As**

$$\inferrule{}{(v = 2) \Rightarrow (v \not = 0)}
            {\bindx{x}{\Int}, \bindx{y}{\Int} \vdash \reftx{v}{v = 2} \preceq \reftx{v}{v \not = 0}}
$$


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

**Q:** Can you fix `calc` so it typechecks?


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

<div class="fragment">
**Rejected** as `n` can be *any* `Nat`

$$0 \leq n \Rightarrow (v = n) \not \Rightarrow (v \not = 0)$$
</div>


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

Postconditions Checked at Return
--------------------------------

<br>

\begin{spec} <div/>
{-@ size    :: [a] -> Pos @-}
size [_]    = 1                        -- (1)
size (_:xs) = 1 + n  where n = size xs -- (2)
\end{spec}

<div class="fragment">
**Verified As**

$$\begin{array}{rll}
\True   & \Rightarrow (v = 1)     & \Rightarrow (0 < v) & \qquad \mbox{at (1)} \\
(0 < n) & \Rightarrow (v = 1 + n) & \Rightarrow (0 < v) & \qquad \mbox{at (2)} \\
\end{array}$$
</div>


Verifying `avg`
---------------

<br>

\begin{code}
avg' xs    = safeDiv total n
  where
    total  = sum  xs
    n      = size xs           -- returns a Pos
\end{code}

<div class="fragment">
**Verifies As**

$$(0 < n) \Rightarrow (v = n) \Rightarrow (v \not = 0)$$
</div>


<br>


Recap
-----

<br>

|                     |                                |
|--------------------:|:-------------------------------|
| **Refinements:**    | Types + Predicates             |
| **Specification:**  | Refined Input/Output Types     |
| **Verification:**   | SMT-based Predicate Subtyping  |


Unfinished Business
-------------------

<br>

How to prevent calling `size` with **empty lists**?

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
