
<div class="hidden">

\begin{code}
{-# LANGUAGE PartialTypeSignatures #-}
{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple"        @-}
{-@ LIQUID "--diff"       @-}
{-@ infixr ++             @-}  -- TODO: Silly to have to rewrite this annotation!

module Opt where

import           Prelude hiding ((++), const, max, or)

asimp       :: AExp -> AExp 
lemma_asimp :: AExp -> State -> _ 

cplus       :: Int -> AExp -> AExp
lemma_cplus :: Int -> AExp -> State -> _

nplus       :: AExp -> AExp -> AExp 
lemma_nplus :: AExp -> AExp -> State -> _ 

norm        :: AExp -> AExp 
lemma_norm  :: AExp -> State -> _ 

(&&&) :: a -> a -> a
x &&& _ = x

-- {-@ lemma_asimp :: a:_ -> s:_ -> {aval (asimp a) s = aval a s} @-}
-- lemma_asimp (AVal _)     _ = ()
-- lemma_asimp (AVar _)     _ = ()
-- lemma_asimp (AAdd a1 a2) s = case (asimp a1, asimp a2) of 
    -- (AVal _, AVal _) -> lemma_asimp a1 s &&& lemma_asimp a2 s 
    -- (b1    , b2    ) -> lemma_asimp a1 s &&& lemma_asimp a2 s



\end{code}
</div>

Case Study: Optimizing Expressions 
----------------------------------

Lets define and "prove correct" some simple code transformations


Recall: State 
-------------

\begin{code}
data State 
  = Emp                   -- ^ Empty  `State`
  | Bind Var Val State    -- ^ Extend `State` by assigning `Var` to `Val`

-- | Lookup the value of `x` in `s`

{-@ reflect get @-}
get :: Var -> State -> Val
get x (Bind y v s) 
  | x == y    = v         -- ^ found the variable `x`, return v
  | otherwise = get x s   -- ^ recursively search `s`
get x Emp     = 0         -- ^ simplification
\end{code}

Recall: Arithmetic Expressions
------------------------------

\begin{code}
type Var = String

type Val = Int

data AExp 
  = AVar Var       -- ^ x
  | AVal Val       -- ^ v
  | AAdd AExp AExp -- ^ e1 + e2 
\end{code}

Recall: Evaluating Expressions in some `State`
----------------------------------------------

\begin{code}
{-@ reflect aval @-}
aval :: AExp -> State -> Val
aval (AVal v)     _  = v
aval (AVar x)     st = get x st
aval (AAdd e1 e2) st = (aval e1 st) + (aval e2 st)
\end{code}

Optimal Expressions
-------------------

**An expression is "optimal" if it has no sub-term `AAdd n1 n2`**

Such subterms should be replaced by `AAdd (n1+n2)`

\begin{code}
{-@ measure optimal @-}
optimal :: AExp -> Bool 
optimal (AVal _)     = True 
optimal (AVar _)     = True 
optimal (AAdd a1 a2) = optimal a1 && optimal a2 
                    && not (const a1 && const a2)

{-@ measure const @-}
const :: AExp -> Bool 
const (AVal _) = True 
const _        = False 
\end{code}

Simplifying Expressions to make them Optimal
--------------------------------------------

\begin{code}
{-@ reflect asimp @-}
{-@ asimp :: _ -> {v:_ | optimal v} @-} 
asimp (AVal n)     = AVal n
asimp (AVar x)     = AVar x  
asimp (AAdd a1 a2) = case (asimp a1, asimp a2) of 
                       (AVal n1, AVal n2) -> AVal (n1 + n2) 
                       (b1     , b2)      -> AAdd b1 b2
\end{code}

Lets Prove that `asimp` is "Correct"
------------------------------------

**Exercise:** How can we _specify_ (and then _verify_) correctness?

\begin{code}
-- {-@ lemma_asimp :: ??? @-}
lemma_asimp _ = undefined
\end{code}

Another Transformation: "Normalizing"
-------------------------------------

**Expressions in _Normal Form_**

`x1 + (x2 + ... (xn + c))`

Sum of variables `x1 ... xn` plus a single constant `c` at the end

Specifying Normal Forms
-----------------------

**Expressions in _Normal Form_**

`x1 + (x2 + ... (xn + c))`

Sum of variables `x1 ... xn` plus a single constant `c` at the end

\begin{code}
{-@ measure is_normal @-}
is_normal :: AExp -> Bool 
is_normal (AAdd a1 a2) = is_var a1 && is_normal a2 
is_normal a            = True 

{-@ measure is_var @-}
is_var :: AExp -> Bool 
is_var (AVar _) = True 
is_var _        = False 

{-@ type NAExp = {a:AExp| is_normal a } @-}
\end{code}

How to Normalize Expressions?
-----------------------------

Lets do it in three steps:

**Step 1**

Add a constant `n` to an `NAExp`

*Step 2**

Add a `NAExp` to another `NAExp`

*Step 3**

Recursively normalize subexpressions and then Step 2 


Step 1: Add a Constant to an `NAExp`
------------------------------------

\begin{code} 
{-@ reflect cplus @-}
{-@ cplus :: Int -> NAExp -> NAExp @-}
cplus n (AAdd a1 a2) = AAdd a1       (cplus n a2)
cplus n (AVar x)     = AAdd (AVar x) (AVal n) 
cplus n (AVal m)     = AVal (n + m)
\end{code}

Step 1: Why is it "correct"?
----------------------------

**Proof follows structure of `lemma_cplus`**

\begin{code}
{-@ lemma_cplus :: n:Int -> a:NAExp -> s:_ -> 
      {aval (cplus n a) s = n + aval a s} 
  @-}
lemma_cplus n (AAdd a1 a2) s = lemma_cplus n a2 s 
lemma_cplus n (AVar _)     _ = () 
lemma_cplus n (AVal _)     _ = () 
\end{code}

Step 2: Add an `NAExp` to another `NAExp`
-----------------------------------------

**This is the tricky one**

\begin{code}
{-@ reflect nplus @-}
{-@ nplus :: NAExp -> NAExp -> NAExp @-} 
nplus (AAdd a1 a2) b = AAdd a1 (nplus a2 b)
nplus (AVal n)     b = cplus n b
nplus (AVar x)     b = AAdd (AVar x) b 
\end{code}

Step 2: Why is it "correct"?
----------------------------

**Proof follows structure of `lemma_nplus`**

\begin{code}
{-@ lemma_nplus :: a:NAExp -> b:NAExp -> s:_ -> 
      {aval (nplus a b) s = aval a s + aval b s} 
  @-}
lemma_nplus (AAdd a1 a2) b s = lemma_nplus a2 b s
lemma_nplus (AVal n)     b s = lemma_cplus n  b s 
lemma_nplus (AVar x)     b s = () 
\end{code}

Step 3: Recursively normalize and `nplus` 
-----------------------------------------

**We did all the hard work already...** 

\begin{code}
{-@ reflect norm @-}
{-@ norm :: AExp -> NAExp @-} 
norm (AAdd a1 a2) = nplus (norm a1) (norm a2)
norm a            = a 
\end{code}


Step 3: Why is it "correct"?
----------------------------

**Proof follows structure of `lemma_nplus`**

\begin{code}
{-@ lemma_norm :: a:_ -> s:_ -> {aval (norm a) s = aval a s} @-}
lemma_norm (AAdd a1 a2) s = lemma_norm a1 s
                        &&& lemma_norm a2 s
                        &&& lemma_nplus (norm a1) (norm a2) s
lemma_norm _ _            = ()
\end{code}
