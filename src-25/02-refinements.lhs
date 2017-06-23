<div class="hidden">

\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--diff"           @-}
{-@ LIQUID "--short-names"    @-}

module SimpleRefinements where
import Prelude hiding (abs, max)

zero, zero', zero'', four, four' :: Int
nats :: [Int]

-- zero'       :: Int
-- safeDiv     :: Int -> Int -> Int
-- size, size' :: [a] -> Int
\end{code}

</div>



Simple Refinement Types
-----------------------

<br>

Refinement Types = **Types** + **Predicates**

Types
-----

<hr style="height:5px; visibility:hidden;" />

\begin{spec}<div/>
b := Int | Bool | ...  -- primitives
   | a, b, c           -- type variables
\end{spec}


Types
-----

<hr style="height:5px; visibility:hidden;" />

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

**Quantifier-Free Logic of Uninterpreted Functions & Linear Arithmetic**


Predicates
----------

**Quantifier-Free Logic of Uninterpreted Functions & Linear Arithmetic**

\begin{spec} <div/>
e := x, y, z, ...         -- variables
   | 0, 1, 2, ...         -- constants
   | e + e | c * e | ...  -- arithmetic
   | f e1 ... en          -- uninterpreted function
\end{spec}

Predicates
----------

**Quantifier-Free Logic of Uninterpreted Functions & Linear Arithmetic**

\begin{spec} <div/>
e := x, y, z, ...         -- variables
   | 0, 1, 2, ...         -- constants
   | e + e | c * e | ...  -- arithmetic
   | f e1 ... en          -- uninterpreted function
\end{spec}

**Uninterpreted Functions**

$$\forall \overline{x}, \overline{y}.\ \overline{x} = \overline{y}\ \Rightarrow\ f(x) = f(y)$$

Predicates
----------

**Quantifier-Free Logic of Uninterpreted Functions & Linear Arithmetic**

\begin{spec} <div/>
e := x, y, z, ...         -- variables
   | 0, 1, 2, ...         -- constants
   | e + e | c * e | ...  -- arithmetic
   | f e1 ... en          -- uninterpreted function
\end{spec}

\begin{spec} <div/>
p := e <= e | ...         -- atoms
   | p && p | p || p | !p -- boolean combinations
\end{spec}


Predicates
----------

Quantifier-Free Logic of Uninterpreted Functions & Linear Arithmetic

<br>

Given a **Verification Condition** (VC)

$$p_1 \Rightarrow p_2$$

SMT solvers can **decide if VC is Valid** ("always true")

Example: Singletons
-------------------

<hr style="height:5px; visibility:hidden;" />

\begin{code}
{-@ type Zero = {v:Int | v == 0} @-}

{-@ zero :: Zero @-}
zero = 0
\end{code}

<div class="fragment">
Refinement types via special comments `{-@ ... @-}`
</div>

Example: Natural Numbers
------------------------

<hr style="height:5px; visibility:hidden;" />

\begin{code}
{-@ type Nat = {v:Int | 0 <= v} @-}

{-@ nats :: [Nat] @-}
nats     =  [0, 1, 2, 3]
\end{code}


A Term Can Have *Many* Types
----------------------------

<div class="mybreak"><br></div>

<div class="fragment">

What *is* the type of `0` ?

<div class="mybreak"><br></div>

\begin{code}
{-@ zero' :: Nat @-}
zero' = zero
\end{code}

</div>


1. Predicate Subtyping [[NUPRL, PVS]](http://pvs.csl.sri.com/papers/subtypes98/tse98.pdf)
----------------------------------

<div class="mybreak"><br></div>

In **environment** $\Gamma$ the type $t_1$ is a **subtype** of $t_2$

$$\boxed{\Gamma \vdash t_1 \preceq t_2}$$


1. Predicate Subtyping [[NUPRL, PVS]](http://pvs.csl.sri.com/papers/subtypes98/tse98.pdf)
----------------------------------


<div class="mybreak"><br></div>

In **environment** $\Gamma$ the type $t_1$ is a **subtype** of $t_2$

$$\boxed{\Gamma \vdash t_1 \preceq t_2}$$

<div class="mybreak"><br></div>

<div class="fragment">

Where **environment** $\Gamma$ is a sequence of *in-scope binders*

$$\Gamma \doteq \overline{\bindx{x_i}{P_i}}$$

</div>


1. Predicate Subtyping [[NUPRL, PVS]](http://pvs.csl.sri.com/papers/subtypes98/tse98.pdf)
--------------------

<div class="mybreak"><br></div>

In **environment** $\Gamma$ the type $t_1$ is a **subtype** of $t_2$

$$\boxed{\Gamma \vdash t_1 \preceq t_2}$$

<div class="mybreak"><br></div>

$$
\begin{array}{rll}
{\mathbf{If\ VC\ is\ Valid}}   & \bigwedge_i p_i \Rightarrow  q  \Rightarrow r & (\mbox{By SMT}) \\
                &  & \\
{\mathbf{Then}} & \overline{\bindx{x_i}{p_i}} \vdash \reft{v}{b}{q} \preceq \reft{v}{b}{r} & \\
\end{array}
$$

Example: Natural Numbers
------------------------

<div class="mybreak"><br></div>

$$
\begin{array}{rcrccll}
\mathbf{VC\ is\ Valid:} & \True     & \Rightarrow &  v = 0   & \Rightarrow &  0 \leq v & \mbox{(by SMT)} \\
\mathbf{So:}            & \emptyset & \vdash      & \Zero    & \preceq     & \Nat      &   \\
\end{array}
$$

Example: Natural Numbers
------------------------

<div class="mybreak"><br></div>

$$
\begin{array}{rcrccll}
\mathbf{VC\ is\ Valid:} & \True     & \Rightarrow &  v = 0   & \Rightarrow &  0 \leq v & \mbox{(by SMT)} \\
\mathbf{So:}            & \emptyset & \vdash      & \Zero    & \preceq     & \Nat      &   \\
\end{array}
$$

<div class="mybreak"><br></div>

<div class="fragment">
And so, we can type:

\begin{code}
{-@ zero'' :: Nat @-}
zero''    =  0    -- as |-  Zero <: Nat
\end{code}
</div>


2. Typing Applications (Function Calls)
---------------------------------------

<div class="mybreak"><br></div>

Terms built up by applications.

<div class="mybreak"><br></div>

\begin{code}
{-@ four :: Nat @-}
four  = x + 1
  where
    x = 3
\end{code}

<div class="mybreak"><br></div>

*How to prove* `four :: Nat` ?

2. Typing Applications (Function Calls)
---------------------------------------

<div class="mybreak"><br></div>

**Dependent Application**

<div class="mybreak"><br></div>

$$\begin{array}{rl}
{\mathbf{If}}   & \Gamma \vdash f   :: \bindx{x}{s} \rightarrow t  \\
                & \Gamma \vdash y   :: s                   \\
{\mathbf{Then}} & \Gamma \vdash f\ y :: t[x \mapsto y]      \\
\end{array}$$

<div class="mybreak"><br></div>

i.e. Output type with *formals substituted by actuals*

2. Typing Applications (Function Calls)
---------------------------------------

<div class="mybreak"><br></div>

**Dependent Application: Example**

<div class="mybreak"><br></div>

$$
\begin{array}{rl}
{\mathbf{If}}   & \Gamma \vdash (+)   :: \bindx{a}{\Int} \rightarrow
                                         \bindx{b}{\Int} \rightarrow
                                         \reft{v}{\Int}{v = a + b} \\
                & \Gamma \vdash x     :: \Int \\
                & \Gamma \vdash 1     :: \Int \\
                &                             \\
{\mathbf{Then}} & \Gamma \vdash x + 1 :: \reft{v}{\Int}{v = x + 1}
\end{array}
$$

2. Typing Applications (Function Calls)
---------------------------------------

<div class="mybreak"><br></div>

And so, we can type:

<div class="mybreak"><br></div>

\begin{code}
{-@ four' :: Nat @-}
four' = x + 1   -- x = 3 |- {v = x+1} <: Nat
  where         -- as
    x = 3       -- x = 3 =>  v = x+1  => 0 <= v
\end{code}

Recap: Refinement Types 101
---------------------------

<div class="mybreak"><br></div>

**Refinement Types**

Types + Predicates

Recap: Refinement Types 101
---------------------------

<div class="mybreak"><br></div>

**Refinement Types**

Types + Predicates

<div class="mybreak"><br></div>

**Refinement Checking**

Dependent Application + Predicate Subtyping


Recap: Refinement Types 101
---------------------------

<div class="mybreak"><br></div>

**Refinement Types**

Types + Predicates

<div class="mybreak"><br></div>

**Refinement Checking**

Dependent Application + Predicate Subtyping

<div class="mybreak"><br></div>

**[[Eliminates boring proofs & Makes Verification Practical]](03-examples.html)**
