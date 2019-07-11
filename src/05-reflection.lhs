<div class="hidden">

\begin{code}
{-# LANGUAGE TupleSections    #-}
{-@ LIQUID "--no-warnings"      @-}
{-@ LIQUID "--short-names"      @-}
{-@ LIQUID "--diff"             @-}
{-@ LIQUID "--exact-data-cons"  @-}
{-@ LIQUID "--higherorder"      @-}
{-@ LIQUID "--automatic-instances=liquidinstances" @-}

-- Hidden code

module Reflection where
import Language.Haskell.Liquid.ProofCombinators
import Prelude hiding (sum, (++))

sillyProof :: Int -> Int -> Proof
appendPf :: [a] -> [a] -> [a] -> ()

{-@ infix   ++ @-}
{-@ reflect ++ @-}

by = (?)

{-
thm :: [a] -> [a] -> [a] -> ()

thm [] ys zs     = ([] ++ ys) ++ zs
                ==. ys ++ zs
                ==. [] ++ (ys ++ zs)
                *** QED
thm (x:xs) ys zs = ((x:xs) ++ ys) ++ zs
                ==. (x : (xs ++ ys)) ++ zs
                ==.  x : ((xs ++ ys) ++ zs)
                ==.  x : (xs ++ (ys ++ zs)) ? thm xs ys zs
                ==.  (x : xs) ++ (ys ++ zs)
                *** QED

 -}
\end{code}

</div>

Types as Theorems, Programs as Proofs
-------------------------------------

<p align=center>
<img src="img/escher-drawing-hands.jpg" height=300px/>
</p>

**Classic Idea** [Curry-Howard][curryhoward], [Propositions As Types][wadler]

Types As Theorems
-----------------

<br>

The Refined _Type_ ...

$$\reft{v}{b}{P}$$

... corresponds to the _Theorem_

$$\mathit{P}$$



Types as Theorems: Example
--------------------------

<br>

The Refined _Type_ ...

\begin{code}
{-@ type OnePlusOneEqualsTwo = {v:() | 1 + 1 = 2} @-}
\end{code}

... corresponds to the _Theorem_

$$1 + 1 = 2$$

Programs As Proofs: Example
---------------------------

<br>

The _Program_ ...

\begin{code}
{-@ easyProof :: OnePlusOneEqualsTwo @-}
easyProof = ()
\end{code}

... corresponds to a _Proof_ that

$$1 + 1 = 2$$

Types As Theorems
-----------------

<br>

The Refined _Type_ ...

$$\bindx{n}{\Nat} \rightarrow \reft{v}{b}{P(n)}$$

... corresponds to the _Theorem_

$$\forall n \in \Nat.\ P(n)$$

Types as Theorems: Example
--------------------------

<br>

The Refined _Type_ ...

\begin{code}
{-@ type PlusCommutes = n:Int -> m:Int -> { n + m = m + n } @-}
\end{code}

... corresponds to the _Theorem_

$$\forall n, m.\ n + m = m + n$$

Programs As Proofs: Example
---------------------------

<br>

The _Program_ ...

\begin{code}
{-@ sillyProof :: PlusCommutes @-}
sillyProof n m = ()
\end{code}

... corresponds to a _Proof_ that

$$\forall n, m.\ n + m = m + n$$


Types as Theorems, Programs as Proofs
-------------------------------------

<br>

<p align=center>

| **Code**    |     | **Math**    |
|------------:|:---:|:------------|
| Types       | are | Theorems    |
| Programs    | are | Proofs      |

</p>


Those Proofs were Boring
------------------------

<br>

**Simple Arithmetic**

Automatically proved by SMT Solver


Those Proofs were Boring
------------------------

<br>

**Simple Arithmetic**

Automatically proved by SMT Solver

<br>

**How about proofs about user-defined functions?**

Beyond automatic SMT, but the user can _write proofs_


Theorems about Functions
------------------------

\begin{code}
{-@ reflect sum @-}
sum :: Int -> Int
sum n
  | n <= 0    = 0
  | otherwise = n + sum (n - 1)
\end{code}

**How can we prove the the theorems**

$\mathit{sum}(1) = 1$,

$\mathit{sum}(2) = 3$,

$\mathit{sum}(3) = 6$.


Refinement Reflection
---------------------

<br>

The _annotation_

\begin{code}
{-@ reflect sum @-}
\end{code}


Automatically gives `sum` the _type_

\begin{spec}
sum :: n:Int -> {v:Int | v = if n == 0 then 0 else n + sum (n-1)}
\end{spec}


Reflect Function into Output Type
---------------------------------

<br>

The type of `sum` connects _implementation_ and _specification_

\begin{spec}
sum :: n:Int -> {v:Int | v = if n == 0 then 0 else n + sum (n-1)}
\end{spec}


Reflect Function into Output Type
---------------------------------

<br>

The type of `sum` connects _implementation_ and _specification_

\begin{spec}
sum :: n:Int -> {v:Int | v = if n == 0 then 0 else n + sum (n-1)}
\end{spec}

<br>

**Key Idea**

Calling `sum n` _reveals_ definition at `n` to refinement logic!



Reflection at Result Type
-------------------------

<br>
\begin{code}
{-@ sum3 :: _ -> { sum 3 == 6 } @-}

sum3 _ = let s0 = sum 0   -- s0 :: {sum 0 = 0        }
             s1 = sum 1   -- s1 :: {sum 1 = 1 + sum 0}
             s2 = sum 2   -- s2 :: {sum 2 = 2 + sum 1}
             s3 = sum 3   -- s3 :: {sum 3 = 3 + sum 2}
         in  ()           -- SMT connects the dots.
\end{code}

**Key Idea**

Calling `sum n` _reveals_ definition at `n` to refinement logic!


Structuring Proofs as Calculations
----------------------------------

<br>

Using combinators from [`ProofCombinators`](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Language/Haskell/Liquid/ProofCombinators.hs)!

<br>
\begin{code}
{-@ sum3' :: _ -> { sum 3 = 6 } @-}
sum3' _ =   sum 3
        ==. 3 + sum 2
        ==. 3 + 2 + sum 1
        ==. 3 + 2 + 1 + sum 0
        ==. 6
        *** QED
\end{code}


Types as Theorems, Programs as Proofs
-------------------------------------

<br>

<p align=center>

| **Code**    |     | **Math**    |
|------------:|:---:|:------------|
| Types       | are | Theorems    |
| Programs    | are | Proofs      |

</p>


Reusing Proofs: Functions as Lemmas
-----------------------------------

<br>

**Proofs are functions**


Reusing Proofs: Functions as Lemmas
-----------------------------------

<br>

**Proofs are functions**

Reuse by _calling_ the function


Reusing Proofs: Functions as Lemmas
-----------------------------------

<br>

**Proofs are functions**

Reuse by _calling_ the function

\begin{code}
{-@ sum4 :: _ -> { sum 4 = 10 } @-}
sum4 _ =  sum 4
      ==. 4 + sum 3
      ==. 4 + 6     ?   sum3 ()   -- lemma { sum 3 == 6 }
      ==. 10
      *** QED
\end{code}

`?` is a library operator (read ``because'')


Types as Theorems, Programs as Proofs
-------------------------------------

<br>

<p align=center>

| **Code**    |     | **Math**    |
|------------:|:---:|:------------|
| Types       | are | Theorems    |
| Programs    | are | Proofs      |

</p>

Types as Theorems, Programs as Proofs
-------------------------------------

<br>

<p align=center>

| **Code**    |     | **Math**    |
|------------:|:---:|:------------|
| Types       | are | Theorems    |
| Programs    | are | Proofs      |
| Functions   | are | Lemmas      |

</p>

Proof by Logical Evaluation
---------------------------

<br>

**Long chains of calculations are tedious**


Proof by Logical Evaluation
---------------------------

<br>

**Long chains of calculations are tedious**

Make the machine do the hard work!

A new algorithm to [emulate computation in SMT logic][popl18]


Proof by Logical Evaluation
---------------------------

<br>

**Long chains of calculations are tedious**

Make the machine do the hard work!

A new algorithm to [emulate computation in SMT logic][popl18]

\begin{code}
{-@ sum3auto :: _ -> { sum 3 = 6 }  @-}
sum3auto _ = ()

{-@ sum4auto :: _ -> { sum 4 = 10 } @-}
sum4auto _ = ()
\end{code}

Proof by Induction
-------------------

<br>

Lets prove the theorem

$$\forall n.\ \sum_{i = 0}^n i = \frac{n \times (n + 1)}{2}$$

Proof by Induction
-------------------

<br>

Lets prove the theorem

$$\forall n.\ \sum_{i = 0}^n i = \frac{n \times (n + 1)}{2}$$

that is

$$\forall n \in \Nat.\ 2 \times \mathit{sum}(n) = n \times (n + 1)$$

Proof by Induction
------------------

$$\forall n \in \Nat.\ 2 \times \mathit{sum}(n) = n \times (n + 1)$$

\begin{code}
{-@ sumPf :: n:Nat -> { 2 * sum n == n * (n + 1) } @-}
sumPf  :: Int -> ()
sumPf 0 =   2 * sum 0
        ==. 0
        *** QED
sumPf n =   2 * sum n
        ==. 2 * (n + sum (n-1))
        ==. 2 * (n + ((n - 1) * n))   ?  sumPf (n - 1)
        ==. n * (n + 1)
        *** QED
\end{code}

**Q:** What happens if we use the wrong induction?


Types as Theorems, Programs as Proofs
-------------------------------------

<br>

<p align=center>

| **Code**    |     | **Math**    |
|------------:|:---:|:------------|
| Types       | are | Theorems    |
| Programs    | are | Proofs      |
| Functions   | are | Lemmas      |

</p>

Types as Theorems, Programs as Proofs
-------------------------------------

<br>

<p align=center>

| **Code**    |     | **Math**    |
|------------:|:---:|:------------|
| Types       | are | Theorems    |
| Programs    | are | Proofs      |
| Functions   | are | Lemmas      |
| Branches    | are | Case-Splits |

</p>

Types as Theorems, Programs as Proofs
-------------------------------------

<br>

<p align=center>

| **Code**    |     | **Math**    |
|------------:|:---:|:------------|
| Types       | are | Theorems    |
| Programs    | are | Proofs      |
| Functions   | are | Lemmas      |
| Branches    | are | Case-Splits |
| Recursion   | is  | Induction   |

</p>

Theorems about Data
-------------------

Recall the list append function:

\begin{code}
(++)        :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
\end{code}

Lets prove that the operator is _associative_

\begin{code}
{-@ type AppendAssoc a = xs:[a] -> ys:[a] -> zs:[a]
                   -> { (xs ++ ys) ++ zs = xs ++ (ys ++ zs) }
  @-}
\end{code}

Theorems about Data: Associativity of `append`
----------------------------------------------

<br>

Lets write fill in a _calculational_ proof:

\begin{code}
{-@ appendPf :: AppendAssoc a @-}
appendPf xs ys zs = () -- Q: Can you help me fill this in?
\end{code}

Case Study: MapReduce
---------------------

<br>
_Chunk_ inputs, _Map_ operation in parallel, and _Reduce_ the results.
<br>

Case Study: MapReduce
---------------------

<br>
_Chunk_ inputs, _Map_ operation in parallel, and _Reduce_ the results.
<br>

<p align=center>
<img src="img/map-reduce.jpg" height=250px/>
</p>


Reduce Theorem
--------------

**Description**

If `op` is associative then `reduce op xs == parallelReduce op xs`

**Theorem**

\begin{spec}
reduceTheorem
 :: op:(a -> a -> a)                  -- for any op-erator
 -> xs:[a]                            -- for any collection xs
 -> Assoc op                          -- if op is associative
 -> {reduce op xs = parReduce op xs}  -- then parReduce is ok!
\end{spec}


Types as Theorems, Programs as Proofs
-------------------------------------

<br>

<p align=center>

| **Code**    |     | **Math**    |
|------------:|:---:|:------------|
| Types       | are | Theorems    |
| Programs    | are | Proofs      |
| Functions   | are | Lemmas      |
| Branches    | are | Case-Splits |
| Recursion   | is  | Induction   |

</p>


Outline
-------

<br>

[Motivation](01-intro.html)

[Refinements 101](02-refinements.html)

[Refinements by Example](03-examples.html)

[How to Avoid Infinite Loops](04-termination.html)

[Types as Theorems, Programs as Proofs](05-reflection.html)

Outline
-------

<br>

[Motivation](01-intro.html)

[Refinements 101](02-refinements.html)

[Refinements by Example](03-examples.html)

[How to Avoid Infinite Loops](04-termination.html)

[Types as Theorems, Programs as Proofs](05-reflection.html)

[Current Status & Future Directions](06-concl.html)


[curryhoward]: https://en.wikipedia.org/wiki/Curry–Howard_correspondence
[wadler]: http://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-types/propositions-as-types.pdf
[popl18]: https://arxiv.org/abs/1711.03842
