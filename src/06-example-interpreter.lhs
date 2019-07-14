
<div class="hidden">

\begin{code}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--no-termination" @-}


module Interpreter (interpret) where

import           Prelude hiding (lookup)
import qualified Data.Set as S

{- CHEAT CODES :-)

{-@ get :: x:_ -> {s:_ | S.member x (vars s)} -> _ @-}

{-@ evalE :: st:_ -> {e:_ | S.isSubsetOf (useE e) (vars st) } -> _ @-}

{-@ evalS :: st:_ 
          -> {s:_ | S.isSubsetOf (useS s) (vars st)} 
          -> {st':_ | S.isSubsetOf (S.union (vars st) (defS s)) (vars st') }
  @-}

isSafe s = useS s == S.empty 

{-@ measure vars @-}
vars :: State -> S.Set Var 
vars (Bind x _ st) = S.union (S.singleton x) (vars st)
vars Emp           = S.empty 

{-@ measure useE @-}
useE :: AExp -> S.Set Var
useE (AVal v)     = S.empty
useE (AVar x)     = S.singleton x
useE (AAdd e1 e2) = S.union (useE e1) (useE e2)

{-@ measure useS @-}
useS :: Stmt -> S.Set Var
useS (Asgn x e)   = useE e 
useS (If e s1 s2) = S.union (useE e) (S.union (useS s1) (useS s2)) 
useS (While e s)  = S.union (useE e) (useS s)
useS (Seq s1 s2)  = S.union (useS s1) (S.difference (useS s2) (defS s1)) 
useS Skip         = S.empty

{-@ measure defS @-}
defS :: Stmt -> S.Set Var
defS (Asgn x e)   = S.singleton x
defS (If e s1 s2) = S.intersection (defS s1) (defS s2)
defS (While e s)  = S.empty
defS (Seq s1 s2)  = S.union (defS s1) (defS s2) 
defS Skip         = S.empty  


-}
\end{code}

</div>

Case Study: Interpreting an Imperative Language
-----------------------------------------------

<br>
<br>


Proving Something is Impossible
-------------------------------

<br>
<br>

**Calls to `impossible` only verify if they _never_ occur at runtime.**

<div class="mybreak"><br></div>

\begin{code}
{-@ impossible :: {v:String | false} -> a @-}
impossible msg = error msg
\end{code}

Proving Something is Impossible
-------------------------------

<br>
<br>

**Exercise:** What are valid inputs for `safeDiv`? 

<div class="mybreak"><br></div>

\begin{code}
safeDiv :: Int -> Int -> Int 
safeDiv x y = if y == 0 
                then impossible "don't divide by zero!" 
                else div x y 
\end{code}

State
-----

A datatype for mapping `Var` to their `Val`ue 

\begin{code}
data State 
  = Emp                   -- ^ Empty  `State`
  | Bind Var Val State    -- ^ Extend `State` by assigning `Var` to `Val`

-- | Update `s` by setting the value of `x` to `v` 
set :: Var -> Val -> State -> State
set x v s = Bind x v s

-- | Lookup the value of `x` in `s`
get :: Var -> State -> Val
get x (Bind y v s) 
  | x == y    = v         -- ^ found the variable `x`, return v
  | otherwise = get x s   -- ^ recursively search `s`
get x Emp     = impossible (x ++ " is undefined!") 

\end{code}

**Exercise:** When is the last case in `get` indeed `impossible`?

Arithmetic Expressions 
----------------------

<br>

**A datatype for representing arithmetic expressions**

Sums of variables and integer constants

\begin{code}
type Var = String
type Val = Int

data AExp 
  = AVar Var       -- ^ x
  | AVal Val       -- ^ v
  | AAdd AExp AExp -- ^ e1 + e2 
\end{code}

Evaluating Arithmetic Expressions
---------------------------------

<br>

**A recursive *evaluator* for Arithmetic Expressions**

\begin{code}
evalE :: State -> AExp -> Val
evalE _  (AVal v)     = v
evalE st (AVar x)     = get x st
evalE st (AAdd e1 e2) = (evalE st e1) + (evalE st e2)
\end{code}

**Exercise:** How to verify the call to `get` is actually safe? 


Variables Read in an Expression
-------------------------------

<div class="mybreak"><br></div>

**Specify the `Var`s we need to `get` to evaluate an `AExp`** 

\begin{code}
{-@ measure useE @-}
useE :: AExp -> S.Set Var
useE (AVar x)     = S.empty    -- TODO: replace with proper definition
useE (AVal v)     = S.empty    -- TODO: replace with proper definition
useE (AAdd e1 e2) = S.empty    -- TODO: replace with proper definition

{- | HINT : You may want to use 
     * S.empty              : the empty set
     * S.singleton x        : the set containing just x 
     * S.union s1 s2        : the union of s1 and s2         
     * S.intersection s1 s2 : the union of s1 and s2         
     * S.isSubsetOf s1 s2   : is s1 a subset of s2 ?
-}
\end{code}

**Exercise:** Now, use `useE` to specify and verify `evalE`.

Statements 
----------

<br>

**A datatype for representing imperative programs**

Assignments, Sequencing, Branching and Loops.

\begin{code}
data Stmt 
  = Asgn  Var AExp       -- ^ x := e
  | If    AExp Stmt Stmt -- ^ if (e != 0) {s1} else {s2}
  | While AExp Stmt      -- ^ while (e != 0) {s}
  | Seq   Stmt Stmt      -- ^ s1; s2
  | Skip                 -- ^ no-op
\end{code}

Evaluating Statements 
---------------------

**A recursive *interpreter* for Statements** 

\begin{code}
evalS :: State -> Stmt -> State

evalS st Skip          = st

evalS st (Asgn x e)    = set x (evalE st e) st

evalS st (If e s1 s2)  = if (evalE st e /= 0)
                           then evalS st s1
                           else evalS st s2

evalS st w@(While e s) = if (evalE st e /= 0)
                           then evalS st (Seq s w) 
                           else st

evalS st (Seq s1 s2)   = evalS (evalS st s1) s2
\end{code}

**Exercise:** When is it safe to call `evalE st e`? 

Specify Used-Before-Defined Variables
-------------------------------------

**Problem: Which `Var`s are *used-before-definition* in a `Stmt`?**

\begin{code}
{-@ measure useS @-}
useS :: Stmt -> S.Set Var
useS (Asgn x e)   = S.empty    -- TODO: replace with proper definition 
useS (If e s1 s2) = S.empty    -- TODO: replace with proper definition
useS (While e s)  = S.empty    -- TODO: replace with proper definition
useS (Seq s1 s2)  = S.empty    -- TODO: replace with proper definition
useS Skip         = S.empty    -- TODO: replace with proper definition

{- | HINT: * S.empty              : the empty set
           * S.singleton x        : the set containing just x 
           * S.union s1 s2        : the union of s1 and s2         
           * S.intersection s1 s2 : the union of s1 and s2         
           * S.isSubsetOf s1 s2   : is s1 a subset of s2 ?          -}
\end{code}

**Exercise:** Now go back and specify and verify `EvalS`

Statically Verifying a Dynamic Check
------------------------------------

<br>

**`interpret` uses a *run-time check* to see if `s` is safe to evaluate.**

\begin{code}
interpret :: Stmt -> Maybe State
interpret s 
  | isSafe s  = Just (evalS Emp s)
  | otherwise = Nothing

{-@ inline isSafe @-}
isSafe :: Stmt -> Bool
isSafe s = True  -- EXERCISE: replace with correct condition
\end{code}

**Exercise:** What run-time check `isSafe` *statically ensures* `evalS Emp s` won't crash?

Some Safe and Unsafe Programs 
-----------------------------

Here are some **safe** statements:

\begin{code}
safe1 = (Asgn "X" (AVal 5))   `Seq`       -- x := 5
        (Asgn "Y" (AVar "X"))             -- y := x

safe2 = (If (AVal 1)                      -- if 1 
          (Asgn "Z" (AVal 1))             --   then z := 1
          (Asgn "Z" (AVal 2))             --   else z := 2
        ) `Seq`                           -- y = z
        (Asgn "Y" (AVar "Z"))
\end{code}

Here are some **unsafe** ones:

\begin{code}
unsafe1 = (Asgn "X" (AVal 5)) `Seq`                   -- x := 5
          (Asgn "Y" (AVar "Z"))                       -- y := z

unsafe2 = (While (AVal 0) (Asgn "Z" (AVal 1)))  `Seq` -- while (0) { z := 1} 
          (Asgn "Y" (AVar "Z"))                       -- y := z 
\end{code}

Plan
----

<br>

**Part I:** [Refinements 101](02-refinements.html)

Case Study: [Vector Bounds](03-example-vectors.html)

**Part II:** [Properties of Structures](04-data-properties.html)

Case Study: [Sorting](05-example-sort.html), [Interpreter](06-example-interpreter.html)

**Part III:** **[Invariants of Data Structures](07-data-legal.html)**

Case Study: [Sorting actually Sorts Lists](08-example-sort.html)

**Part IV:** [Termination](09-termination.html) and [Correctness Proofs](10-reflection.html)

Case Study: [Optimizing Arithmetic Expressions](11-example-opt.html)
