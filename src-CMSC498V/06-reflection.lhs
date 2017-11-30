Programs are Proofs
---------------------

<br>
<br>

Each time you write a program, you are writing a proof! 

<br>

Proof of what?
<br>



Each program proves its type
-----------------------------

<br>
<br>

For each `Int` there exists an `Int`:

\begin{spec}
idInt :: Int -> Int 
idInt x = x 
\end{spec}

<br>


Each program proves its type
-----------------------------

<br>
<br>

For each `a` there exists an `a`:

\begin{spec}
id :: a -> a 
id x = x 
\end{spec}

<br>

Each program proves its type
-----------------------------

<br>

But, for each `Int` there exists an `a`:

\begin{spec}
idErr :: Int -> a  
idErr x = error "Define me?"
\end{spec}

<br>

Programs (as proofs) should be well-formed ...

... that is terminating and total.



Each program proves its type
-----------------------------

<br> 

Known as [Curry–Howard correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence)

independently developed by Curry (1934) and Howard (1969). 

<br> 

Known as [Propositions as Types & Proofs as Programs](http://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-types/propositions-as-types.pdf)

by Phil Wadler (2014).


Propositions as Refinement Types
--------------------------------

<br>

**Q:** Can you read the following type as a theorem?

\begin{spec}
f :: x:Int -> y:Int -> {v:() | x + y = y + x}
\end{spec}

<br>
**Q:** How would you prove this theorem?

<br>



<div class="hidden">
e.g. Parallelized Function Equivalent to Original

<br>
<p align="center">
A. Farmer *et al*: Reasoning with the HERMIT
<br><br>
<img src="http://goto.ucsd.edu/~nvazou/images/hermit_laws.png" alt="Hermit Laws" style="width: 350px;" align="middle" />
</p>
</div>

Propositions as Refinement Types
-----------------

<br>


Refined Types are **theorems**

and

Haskell Functions are **proofs**.



\begin{code}
{-# LANGUAGE TupleSections    #-}
module ProgramsAsProofs where
import Language.Haskell.Liquid.ProofCombinators
\end{code}


\begin{code}
{-@ onePlusOne :: () -> {v:() | 1 + 1 == 2 } @-}
onePlusOne _ = ()
\end{code}


Make the theorems pretty!
-------------------------

<br>

[`ProofCombinators`](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Language/Haskell/Liquid/ProofCombinators.hs) comes with Liquid Haskell
and allows for pretty proofs!

<br>
\begin{code}
-- import Language.Haskell.Liquid.ProofCombinators

{-@ propOnePlusOne :: () ->  {v: Proof | 1 + 1 == 2} @-}
propOnePlusOne _ = trivial
\end{code}


Make the theorems even prettier!
--------------------------------

<br>

[`ProofCombinators`](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Language/Haskell/Liquid/ProofCombinators.hs) comes with Liquid Haskell
and allows for pretty proofs!

<br>
\begin{code}
{-@ propOnePlusOne' :: _ ->  { 1 + 1 == 2 } @-}
propOnePlusOne' _ = trivial *** QED
\end{code}


Use more SMT knowledge
----------------------

<br>

[`ProofCombinators`](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Language/Haskell/Liquid/ProofCombinators.hs) comes with Liquid Haskell
and allows for pretty proofs!

<br>
\begin{code}
{-@ propPlusAccum :: x:Int -> y:Int -> { x + y == y + x } @-}
propPlusAccum _ _ = trivial *** QED
\end{code}



Theorems about Haskell functions
--------------------------------

<br>
<br>
<br>

Express & Prove Theorems **in** Haskell ...

... **for** Haskell functions.

Define (terminating) `fib` in the logic!
-------------------------------------

**Reflect** `fib` in the logic.

\begin{code}
{-@ reflect fib @-}
{-@ fib :: i:Nat -> Nat @-}
fib i | i == 0    = 0
      | i == 1    = 1
      | otherwise = fib (i-1) + fib (i-2)
\end{code}

<br>

(Need to prove `fib` terminates...)

Structuring Proofs
------------------

<br>
<br>
Using combinators from [`ProofCombinators`](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Language/Haskell/Liquid/ProofCombinators.hs)!

<br>
<br>
\begin{code}
{-@ fibTwo :: _ -> { fib 2 == 1 } @-}
fibTwo _
  =   fib 2
  ==. fib 1 + fib 0
  *** QED
\end{code}



Reusing Proofs: The "because" operator
--------------------------------------

<br>
<br>

Using combinators from [`ProofCombinators`](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Language/Haskell/Liquid/ProofCombinators.hs)!

<br>
<br>
\begin{code}
{-@ fibThree :: _ -> { fib 3 == 2 } @-}
fibThree _
  =   fib 3
  ==. fib 2 + fib 1
  ==. 1     + 1      ? fibTwo ()
  ==. 2
  *** QED
\end{code}

Paper & Pencil style Proofs
---------------------------

<br>
`fib` is increasing
<br>

\begin{code}
{-@ fibUp :: i:Nat -> {fib i <= fib (i+1)} @-}
fibUp i
  | i == 0
  =   fib 0 <. fib 1
  *** QED
  | i == 1
  =   fib 1 <=. fib 1 + fib 0 <=. fib 2
  *** QED
  | otherwise
  =   fib i
  ==. fib (i-1) + fib (i-2)
  <=. fib i     + fib (i-2) ? fibUp (i-1)
-- 
-- Missing step here !!!
-- 
  *** QED
\end{code}


Another "Paper & Pencil" Proof
------------------------------

<br>
**Exercise:** Lets fix the proof that `fib` is monotonic?
<br>

\begin{code}
{-@ fibMonotonic :: x:Nat -> y:{Nat | x < y } -> {fib x <= fib y}  @-}
fibMonotonic x y
  | y == x + 1
  =   fib x
  <=. fib (x+1) ? fibUp x
  <=. fib y
  *** QED
  | x < y - 1
  =   fib x
  <=. fib (y-1) ? trivial {- Inductive Hypothesis call goes here -}
  <=. fib y     ? fibUp (y-1)
  *** QED
\end{code}


**Exercise:** Can you replace `trivial` to fix the monotonicity proof?
<br>


Generalizing monotonicity proof
-------------------------------

<br>
**Exercise:** Generalize the implementation of `fMono` proof below to any increasing function `f`.
<br>

\begin{code}
{-@ fMono :: f:(Nat -> Int)
          -> fUp:(z:Nat -> {f z <= f (z+1)})
          -> x:Nat
          -> y:{Nat|x < y}
          -> {f x <= f y} / [y] @-}
fMono f fUp x y
  | y == x + 1
  =   fib x
  <=. fib (x+1) ? fibUp x
  <=. fib y
  *** QED
  | x < y - 1
  =   fib x
  <=. fib (y-1) ? fibMonotonic x (y-1)
  <=. fib y     ? fibUp (y-1)
  *** QED
\end{code}

Reusing Theorems by Application
-------------------------------

<br>

`fib` is monotonic!

<br>

\begin{code}
{-@ fibMono :: n:Nat -> m:{Nat | n < m }  -> {fib n <= fib m} @-}
fibMono     = fMono fib fibUp
\end{code}

Recap
-----

<br>

|                     |                                |
|--------------------:|:-------------------------------|
| **Refinements:**    | Types + Predicates             |
| **Specification:**  | Refined Input/Output Types     |
| **Verification:**   | SMT-based Predicate Subtyping  |
| **Measures:**       | Specify Properties of Data     |
| **Termination:**    | Well-founded Metrics           |
| **Programs as Proofs:**| Theorem Proving in Haskell  |


<br>

<div class="fragment">
**Next:** [Structural Induction](07-structural-induction.html): Program Properties about data types!
</div>


Appendix
--------

\begin{code}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--higherorder"     @-}

fib :: Int -> Int
propPlusAccum :: Int -> Int -> Proof
propOnePlusOne :: () -> Proof
onePlusOne :: () -> Proof
fibTwo :: () -> Proof
fibUp :: Int -> Proof
fibThree :: () -> Proof
fMono :: (Int -> Int)
      -> (Int -> Proof)
      -> Int
      -> Int
      -> Proof
fibMono :: Int -> Int -> Proof
fibMonotonic :: Int -> Int -> Proof
\end{code}