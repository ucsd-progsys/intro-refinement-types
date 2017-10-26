Refinement Reflection
---------------------
<br>

Reflect function definition into its result refined type.

<br>

\begin{code}
module RefinementRelfection where
{-@ LIQUID "--higherorder"    @-}
import Language.Haskell.Liquid.ProofCombinators 
\end{code}

<br>

Refinement Types
---------------------

Types + Logical Predicates

\begin{code}
{-@ type Nat = {v : Int | 0 <= v} @-}
\end{code}

Used for Specification

\begin{code}
{-@ nats :: [Nat] @-}
nats = [0, 1, 2]
\end{code}


Specification & Termination & Verification 
-------------------------------------------

We user refinement types for specification & termination 

\begin{code}
{-@ fib :: Nat -> Nat @-}

fib i | i <= 1 = i 
fib i = fib (i-1) + fib (i-2)
\end{code}


Propositions
------------

Propositions are refined unit types

\begin{spec} <div/>
  type Proof = ()
\end{spec}

with SMT-automated proofs 

\begin{code}
{-@ plus_2_2 :: () -> {v:Proof | 2 + 2 = 4 } @-}
plus_2_2 _   = () 
\end{code}


Universal & Existential Propositions
-------------------------------------

Function arguments for universals 
\begin{code}
{-@ plusComm :: x:Int -> y:Int 
             -> {v:Proof | x + y = y + x } @-}
plusComm _ _ = () 
\end{code}

<br>

Dependent Pairs for existentials
\begin{code}
{-@ intUp :: n:Int -> (m::Int, {v:Proof | n < m}) @-}
intUp n = (n+1, ()) 
\end{code}


Refinement Reflection
------------------------------

<br>

How do we talk about user defined functions
e.g., `fib`?

<br>

\begin{code}
{-@ reflect fib @-}
\end{code}


Step 1: Definition
--------------------------


In the logic, `fib` is an unintepreted function 

\begin{code}
{-@ fibCongr :: x:Nat -> y:Nat 
             -> {x == y => fib x == fib y} @-}
fibCongr _ _ = ()
\end{code}


Step 2: Reflection
-------------------

The Haskell definition of `fib`
is reflected into the type

\begin{spec}
  fib :: i:Nat -> { v:Nat | v = fib i && fibP i }
\end{spec}

where
\begin{spec}
  fibP i = if i <= 1 then fib i = i 
           else fib i = fib (i-1) + fib (i-2)
\end{spec}


Step 3: Application 
-------------------

<br>
To prove that `fib 2 == 1` we  call `fib` on `0`, `1`, and `2`.

\begin{code}
{-@ pf_fib2 :: { v:_ | fib 2 = 1 } @-}
pf_fib2 = let { t0 = fib 0; t1 = fib 1; t2 = fib 2 } 
          in (t0,t1,t2)
\end{code}

<br> 
This does not look good ...


Proof structure is not important ...
---------------------------------

... as long as it has proper applications.

\begin{code}
{-@ pf_fib2' :: () -> {v:[Nat] | fib 2 = 1 } @-}
pf_fib2' _   =  [ fib 0 , fib 1 , fib 2 ]
\end{code}

<br> 
Still does not look good ...



Equational Proofs
----------------------

[ProofCombinators](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Language/Haskell/Liquid/ProofCombinators.hs) library to structure proof terms.

\begin{spec} </div>
data QED = QED 

(***) :: a -> QED -> Proof 
_ *** QED = () 
\end{spec}

\begin{spec}
(==.) :: x:a -> a -> {v:a | v == x }
\end{spec}


Equational Proofs in Practice 
----------------------

We rewrite the `fib2` proof using [ProofCombinators](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Language/Haskell/Liquid/ProofCombinators.hs).


\begin{code}
{-@ fib2_1 :: () -> { fib 2 = 1 } @-}
fib2_1 _   
  =   fib 2 
  ==. fib 1 + fib 0 
  ==. 1 
  *** QED
\end{code}

<br> 
Finally, looks like a proof! 

“Because” Combinators
---------------------------

We extend `(==.)` to take an optional proof argument and ...

... define a dollar like `(?)` combinator. 

\begin{code}
{-@ fib3_2 :: () -> { fib 3 = 2 } @-}
fib3_2 _ 
  =   fib 3 
  ==. fib 2 + fib 1 
  ==. 2             ? fib2_1 ()
  *** QED
\end{code}


Arithmetic and Ordering
------------------------

Similarly, we use arithmetic proof combinators 
for arithmetic proofs. 

\begin{code}
{-@ fibUp :: n:Nat -> {fib n <= fib (n + 1)} @-}
fibUp 0 
  = fib 0  <. fib 1 *** QED
fibUp 1 
  = fib 1 <=. fib 1 + fib 0 ==. fib 2 *** QED
fibUp n 
  = fib n <=. fib n + fib (n-1) ==. fib (n+1) *** QED
\end{code}


Induction
------------------------------------

Since `fib` increases locally, it is also monotonic. 
\begin{code}
{-@ fibMono :: x:Nat -> y:{Nat | x < y} 
            ->  {fib x <= fib y} / [y] @-}
fibMono x y
  | x+1 == y 
  = fib x <=. fib (x+1) ? fibUp x 
          <=. fib y 
          *** QED
  | x+1 <  y 
  = fib x <=. fib (y-1) ? fibMono x (y-1) 
          <=. fib y     ? fibUp (y-1) 
          *** QED
\end{code}


Higher Order Reasoning
------------------------------------

Every function that increases locally, is also monotonic. 
\begin{code}
{-@ fMono :: f:(Nat -> Int) 
          -> (n:Nat -> {f n <= f (n + 1)})
          -> x:Nat -> y:{Nat | x < y} 
          ->  {f x <= f y} / [y] @-}
fMono f up x y
  | x+1 == y 
  = f x <=. f (x+1) ? up x 
        <=. f y 
        *** QED
  | x+1 <  y 
  = f x <=. f (y-1) ? fMono f up x (y-1) 
        <=. f y     ? up (y-1) 
        *** QED
\end{code}



Instantiation of Higher Order Theorems
------------------------------------

We get back monotonicity of `fib` ...

... by application of the general `fMono` theorem.

\begin{code}
{-@ fibMono' :: n:Nat -> m:{Nat |  n < m } -> { fib n <= fib m } @-}
fibMono' = fMono fib fibUp
\end{code}

What about automation?
-----------------------------------------------------------



<br>

**Observation:** 
Since all reflected functions are terminating, ...

... their unfoldings are also termining, 

... so unfolding can be **predictably** automated. 


Proof by Logical Evaluation (*PLE*)
--------------------------------


- **In theory:** Repeatedly unfold each function that statically unfolds.

- **In practise:** 

\begin{code}
{-@ LIQUID "--automatic-instances=liquidinstanceslocal" @-}

{- automatic-instances fib3_2_PLE @-}
{- fib3_2_PLE :: () -> { fib 3 = 2 } @-}
fib3_2_PLE _   =  ()
\end{code}


Summary: 
---------

<br>

- Refinement Reflection and Proof by Logical Evaluation combined ...

- ... allow for complete verification with SMT-automation!

<br>

- Case Study: [**MapReduce Equivalence**](03-laws-for-lists.html)





Haskell Sigs
--------------

\begin{code}
fib3_2_PLE     :: () -> Proof
nats      :: [Int]
fib       :: Int -> Int
fibCongr  :: Int -> Int -> Proof
plus_2_2  :: () -> Proof 
pf_fib2   :: (Int,Int,Int)
pf_fib2'  :: () -> [Int]
fib2_1     :: () -> Proof 
intUp     :: Int -> (Int, Proof)
plusComm  :: Int -> Int -> Proof 
fib3_2     :: () -> Proof
fibUp :: Int -> Proof 
fMono :: (Int -> Int)
      -> (Int -> Proof)
      -> Int -> Int
      -> Proof
fibMono' :: Int -> Int -> Proof
fibMono :: Int -> Int -> Proof
\end{code}

