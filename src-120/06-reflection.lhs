<div class="hidden">

\begin{code}
{-# LANGUAGE TupleSections    #-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--diff"           @-}


-- Hidden code
{-@ LIQUID "--higherorder"     @-}

module RefinementReflection where
import Language.Haskell.Liquid.ProofCombinators

fib :: Int -> Int
propPlusAccum :: Int -> Int -> Proof
propOnePlusOne :: () -> Proof
onePlusOne :: () -> Proof
fibOne :: () -> Proof
fibTwo :: () -> Proof
fibEq  :: () -> Proof
fibCongr :: Int -> Int -> Proof
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

</div>

<br>
<br>
<br>
<br>
<br>



Refinement Reflection
---------------------

<br>
<br>
Allow terminating **Haskell** functions in refinements!


Theorems about Haskell functions
--------------------------------

<br>

e.g. Parallelized Function Equivalent to Original

<br>


\begin{spec}
forall xs. sum xs == parallelSum xs
\end{spec}

<div class="hidden">

<br>
<p align="center">
A. Farmer *et al*: Reasoning with the HERMIT
<br><br>
<img src="http://goto.ucsd.edu/~nvazou/images/hermit_laws.png" alt="Hermit Laws" style="width: 350px;" align="middle" />
</p>
</div>


Theorems about Haskell functions
--------------------------------

<br>
<br>
<br>
Can we express the above theorems in Liquid Haskell?
<br>
<br>

Express & Prove Theorems **in** Haskell ...

... **for** Haskell functions.



Types As Theorems
-----------------


Refined Types are **theorems**

and

Haskell Functions are **proofs**.

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
Can we express the above theorems in Liquid Haskell?
<br>
<br>

Express & Prove Theorems **in** Haskell ...

... **for** Haskell functions.

Refinement Reflection
---------------------

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

`fib` is an uninterpreted function
----------------------------------

<br>
For which logic only knows the congruence axiom...
<br>

\begin{code}
{-@ fibCongr :: i:Nat -> j:Nat -> {i == j => fib i == fib j} @-}
fibCongr _ _ = trivial
\end{code}

<br>

... and nothing else

<br>
\begin{code}
{-@ fibOne :: () ->  {fib 1 == 1 } @-}
fibOne _ = trivial
\end{code}

Reflect Function into Output Type
---------------------------------

<br>
The type of `fib` connects logic & Haskell implementation
<br>

\begin{spec}
fib :: i:Nat
    -> {v:Nat |  v == fib i
              && v == if i == 0 then 0 else
                      if i == 1 then 1 else
                      fib (i-1) + fib (i-2)
       }
\end{spec}

<br>

**Calling** `fib i` reveals its implementation in the logic!

Reflection at Result Type
-------------------------

<br>
\begin{code}
{-@ fibEq :: () ->  {fib 1 == 1 } @-}
fibEq _ = let f1 = fib 1 -- f1:: {f1 == fib 1 && f1 == 1}
          in [f1] *** QED
\end{code}

<br>

**Exercise:** Lets prove that `fib 2 == 1`.

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
  <=. fib i     + fib (i-1) ? fibUp (i-2)
  <=. fib (i+1)
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

Note: Totality checker should be on for valid proofs

\begin{code}
{-@ LIQUID "--totality" @-}
\end{code}


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
| **Reflection:**     | Haskell functions in Logic     |


<br>

<div class="fragment">
**Next:** [Structural Induction](07-structural-induction.html): Program Properties about data types!
</div>
