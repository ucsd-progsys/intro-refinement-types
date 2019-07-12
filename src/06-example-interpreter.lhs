
<div class="hidden">

\begin{code}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--no-termination" @-}


module Interpreter (interpret) where

import           Prelude hiding (lookup)
import qualified Data.Set as S
\end{code}

</div>

Case Study: Interpreting an Imperative Language
-----------------------------------------------

<br>
<br>



Proving Something (Bad) is Impossible
--------------------------------------

In particular, this corresponds to establishing that the call to impossible
*never* happens at run time, by verifying that the below typechecks:

\begin{code}
{-@ impossible :: {v:String | false} -> a @-}
impossible msg = error msg
\end{code}

State
-----

\begin{code}
data State 
  = Emp                 -- ^ Empty state
  | Bind Var Val State  -- ^ `Bind x v s` extends `s` by assigning `x` the value `v` 

set :: Var -> Val -> State -> State
set x v s = Key x v s

get :: Var -> State -> Val
get x (Key y v s) 
  | x == y        = v         -- found the variable `x`, return v
  | otherwise     = get x s   -- recursively search `s`
get x Emp         = impossible (x ++ " is undefined!") 
\end{code}

**Exercise: How to verify the last case in `get` is indeed `impossible`? 

Arithmetic Expressions 
----------------------

\begin{code}
type Var = String
type Val = Int

data AExp 
  = AVar Var       -- x
  | AVal Val       -- v
  | Add  AExp AExp -- e1 + e2 
\end{code}

Evaluating Arithmetic Expressions
---------------------------------

\begin{code}
evalE :: State -> AExp -> Val
evalE _ (Val v)     = v
evalE s (Var x)     = get x s
evalE s (Add e1 e2) = (evalE st e1) + (evalE st e2)
\end{code}

**Exercise: How to verify the call to `get` is actually safe? 

Variables Read in an Expression
-------------------------------

Lets describe which variables we will run `get` on?

\begin{code}
{-@ measure readE @-}
readE :: AExp -> S.Set Variable   
readE (Var x)     = S.empty    -- TODO: replace with proper definition
readE (Val v)     = S.empty    -- TODO: replace with proper definition
readE (Add e1 e2) = S.empty    -- TODO: replace with proper definition
\end{code}

**Exercise:** Go back and use `readE` to specify and verify `evalE`.

Statements 
----------

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

\begin{code}
evalS :: State -> Stmt -> State
evalS st Skip          = st
evalS st (Asgn x e)    = set x (evalE st e)
evalS st (If e s1 s2)  = if (evalE st e /= 0)
                           then evalS st s1
                           else evalS st s2
evalS st w@(While e s) = if (evalE st e /= 0)
                           then evalS (evalS st s) w
                           else st
evalS st (Seq s1 s2)   = evalS (evalS st s1) s2
\end{code}

**Exercise:** How to verify that the call to `evalE` is safe?

Definition-before-Use 
---------------------

**Problem: Ensure a `Var` is *defined before it is used* in an `AExp`?**

Require a measure for the `Set` of **free** variables 

That are **used-before-definition** `Statement`

\begin{code}
{-@ measure readS @-}
readS :: Statement -> S.Set Variable
readS (Assign x e)     = S.empty    -- TODO: replace with proper definition 
readS (IfZ e s1 s2)    = S.empty    -- TODO: replace with proper definition
readS (WhileZ e s)     = S.empty    -- TODO: replace with proper definition
readS (Sequence s1 s2) = S.empty    -- TODO: replace with proper definition
readS Skip             = S.empty    -- TODO: replace with proper definition
\end{code}

**Exercise:** Now go back and specify and verify `EvalS`

A Safe Interpreter 
------------------

\begin{code}
interpret :: Stmt -> Maybe State
interpret s 
  | isSafe s  = Just (evalS Emp s) -- `s` does not use any vars before definition 
  | otherwise = Nothing            -- `s` may use some var before definition

{-@ inline isSafe @-}
isSafe :: Stmt -> Bool
isSafe s = True                    -- EXERCISE: replace with correct implementation 
\end{code}

**Exercise:** What is a non-trivial (i.e. not `False`) that allows us 
to safely interpret programs?

Some Safe Programs 
------------------

Here are some **safe** statements:

\begin{code}
safe1 = (Assign "X" (Val 5)) 
        `Seq` 
        (Assign "Y" (Var "X"))

safe2 = (If (Val 1) 
          (Asgn "Z" (Val 1)) 
          (Asgn "Z" (Val 2))
        )
        `Seq`
        (Asgn "Y" (Var "Z"))
\end{code}

Some Unsafe Programs 
--------------------

Here are some **unsafe** ones:

\begin{code}
unsafe1 = (Asgn "X" (Val 5)) 
          `Seq` 
          (Asgn "Y" (Var "Z"))

unsafe2 = (While (Val 1) (Asgn "Z" (Val 1)))
          `Seq`
          (Asgn "Y" (Var "Z"))
\end{code}