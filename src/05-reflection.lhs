<div class="hidden">

\begin{code}
{-# LANGUAGE TupleSections    #-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--diff"           @-}


-- Hidden code
{-@ LIQUID "--higherorder"     @-}
{-@ LIQUID "--automatic-instances=liquidinstances" @-}

module RefinementReflection where
import Language.Haskell.Liquid.ProofCombinators

sillyProof :: Int -> Int -> Proof
\end{code}

</div>

Types as Theorems, Programs as Proofs
-------------------------------------

<p align=center>
<img src="img/escher-drawing-hands.gif" height=300px/>
</p>

**Classic Idea**

[Curry-Howard][curryhoward], [Propositions As Types][wadler15]

Types As Theorems
-----------------

<br>

The Refined _Type_ ...

$$\reft{v}{b}{P}$$

... corresponds to the _theorem_

$$\mathit{P}$$



Types as Theorems: Example
--------------------------

<br>

The Refined _Type_ ...

\begin{code}
{-@ type OnePlusOneEqualsTwo = {v:() | 1 + 1 = 2} @-}
\end{code}

... corresponds to the _theorem_

$$1 + 1 = 2$$

Programs As Proofs: Example
---------------------------

<br>

The _program_ ...

\begin{code}
{-@ easyProof :: OnePlusOneEqualsTwo @-}
easyProof = ()
\end{code}

... corresponds to a _proof_ that

$$1 + 1 = 2$$

Types As Theorems
-----------------

<br>

The Refined _Type_ ...

$$\bindx{n}{\Nat} \rightarrow \reft{v}{b}{P(n)}$$

... corresponds to the _theorem_

$$\forall n \in \Nat.\ P(n)$$

Types as Theorems: Example
--------------------------

<br>

The Refined _Type_ ...

\begin{code}
{-@ type PlusCommutes = n:Int -> m:Int -> { n + m = m + n } @-}
\end{code}

... corresponds to the _theorem_

$$\forall n, m.\ n + m = m + n$$

Programs As Proofs: Example
---------------------------

<br>

The _program_ ...

\begin{code}
{-@ sillyProof :: PlusCommutes @-}
sillyProof n m = ()
\end{code}

... corresponds to a _proof_ that

$$\forall n, m.\ n + m = m + n$$

Those Proofs were Boring
------------------------

<br>

Simple Arithmetic, automatically proved by SMT Solver


Those Proofs were Boring
------------------------

<br>

Simple Arithmetic, automatically proved by SMT Solver

**How about proofs about user-defined functions?**

Theorems about Functions
------------------------

<br>

\begin{code}
{-@ reflect sum @-}
sum :: Int -> Int
sum n
  | n <= 0    = 0
  | otherwise = n + sum (n - 1)
\end{code}

<br>

**How can we prove the the theorems**

$\mathit{sum}(0) = 0$, $\mathit{sum}(1) = 1$, $\mathit{sum}(2) = 3$, $\mathit{sum}(3) = 6$


Refinement Reflection
---------------------

<br>

The annotation

\begin{code}
{-@ reflect sum @-}
\end{code}

tells LiquidHaskell to type the function as:


Reflect Function into Output Type
---------------------------------

The type of `fib` connects logic & Haskell implementation

<br>

\begin{spec}
sum :: n:Int -> {v:Int | v = (if n == 0 then 0 else n + sum (n-1)) }
\end{spec}

<br>

**Key Idea**

Calling `sum n` _reveals implementation_ at `n` to refinement logic!

Reflection at Result Type
-------------------------

<br>
\begin{code}
{-@ sum3 :: _ -> { sum 3 == 6 } @-}
sum3 _ = let s0 = sum 0  -- s0 :: {s0 = sum 0 && s0 = 0}
             s1 = sum 1  -- s1 :: {s1 = sum 1 && s1 = 1 + sum 0}
             s2 = sum 2  -- s2 :: {s2 = sum 2 && s2 = 1 + sum 1}
             s3 = sum 3  -- s3 :: {s3 = sum 3 && s3 = 1 + sum 2}
         in
         [s0,s1,s2,s3] *** QED  -- SMT solver connects the dots.
\end{code}

<br>

**Key Idea**

Calling `sum n` _reveals implementation_ at `n` to refinement logic!


Structuring Proofs as Calculations
----------------------------------

<br>

Using combinators from [`ProofCombinators`](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Language/Haskell/Liquid/ProofCombinators.hs)!

<br>
\begin{code}
{-@ sum3' :: _ -> { sum 3 == 6 } @-}
sum3' _ =   sum 3
        ==. 3 + sum 2
        ==. 3 + 2 + sum 1
        ==. 3 + 2 + 1 + sum 0
        ==. 6
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

[curryhoward]: https://en.wikipedia.org/wiki/Curry–Howard_correspondence
[wadler]: http://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-types/propositions-as-types.pdf
