<div class="hidden">

\begin{code}
{-# LANGUAGE TupleSections #-}
{-@ LIQUID "--no-warnings" @-}
{-@ LIQUID "--short-names" @-}
{-@ LIQUID "--reflection"  @-}
{-@ LIQUID "--ple"         @-}

-- Hidden code

module Reflection where
-- import Language.Haskell.Liquid.ProofCombinators
import Prelude hiding (sum, (++))

sum        :: Int -> Int
sillyProof :: Int -> Int -> ()
appendPf   :: [a] -> [a] -> [a] -> ()
thm_sum    :: Int -> Int

{-@ infix   ++ @-}
{-@ reflect ++ @-}


infixl 3 ===
{-@ (===) :: x:a -> y:{a | y == x} -> {v:a | v == x && v == y} @-}
(===) :: a -> a -> a
_ === y  = y

infixl 3 ?

{-@ (?) :: forall a b <pa :: a -> Bool, pb :: b -> Bool>. a<pa> -> b<pb> -> a<pa> @-}
(?) :: a -> b -> a 
x ? _ = x 
{-# INLINE (?)   #-} 

by = (?)

{-
{-@ thm :: AppendAssoc a @-}
thm :: [a] -> [a] -> [a] -> [a]

thm [] ys zs     = ([] ++ ys) ++ zs
               === ys ++ zs
               === [] ++ (ys ++ zs)

thm (x:xs) ys zs = ((x:xs) ++ ys) ++ zs
               === (x : (xs ++ ys)) ++ zs 
               ===  x : ((xs ++ ys) ++ zs)
                 ? thm xs ys zs
               ===  x : (xs ++ (ys ++ zs)) 
               === (x : xs) ++ (ys ++ zs)
-}
\end{code}

</div>

Types as Propositions, Programs as Proofs
-----------------------------------------

<p align=center>
<img src="img/escher-drawing-hands.jpg" height=300px/>
</p>

**A (Terminating) Program is a Proof**

Classic Idea [Curry-Howard][curryhoward], [Propositions As Types][wadler]

Types As Propositions 
---------------------

<br>

The Refined _Type_ ...

$$\reft{v}{b}{P}$$

... corresponds to the _Proposition_

$$\mathit{P}$$


Types as Propositions: Example
------------------------------

<br>

The Refined _Type_ ...

\begin{code}
{-@ type OnePlusOneEqualsTwo = {v:() | 1 + 1 = 2} @-}
\end{code}

... corresponds to the _Proposition_

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

Types as Propositions
---------------------

<br>

The Refined _Type_ ...

$$\bindx{n}{\Nat} \rightarrow \reft{v}{b}{P(n)}$$

... corresponds to the _Proposition_

$$\forall n \in \Nat.\ P(n)$$

Types as Propositions: Example
--------------------------

<br>

The Refined _Type_ ...

\begin{code}
{-@ type PlusCommutes = n:Int -> m:Int -> { n + m = m + n } @-}
\end{code}

... corresponds to the _Proposition_

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


Types as Propositions, Programs as Proofs
-------------------------------------

<br>

<p align=center>

| **Code**    |     | **Math**     |
|------------:|:---:|:-------------|
| Types       | are | Propositions |
| Programs    | are | Proofs       |

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
{-@ sum :: n:Nat -> Int @-}
sum 0 = 0
sum n = n + sum (n-1)
\end{code}

**How can we prove the the theorems**

$\mathit{sum}(1) = 1$,

$\mathit{sum}(2) = 3$,

$\mathit{sum}(3) = 6$.


Refinement Reflection
---------------------

<br>

The _annotation_

\begin{spec}<div/>
{-@ reflect sum @-}
\end{spec}

Automatically gives `sum` the _type_

\begin{spec}
sum :: n:Int -> {v:Int | v = if n == 0 then 0 else n + sum (n-1)}
\end{spec}

... but `sum` is **uninterpreted** in the refinement logic.

Reflect Function into Output Type
---------------------------------

<br>

**The type of `sum` connects _implementation_ and _specification_**

\begin{spec}
sum :: n:Int -> {v:Int | v = if n == 0 then 0 else n + sum (n-1)}
\end{spec}

... but `sum` is **uninterpreted** in the refinement logic.


Reflect Function into Output Type
---------------------------------

<br>

The type of `sum` connects _implementation_ and _specification_

\begin{spec}
sum :: n:Int -> {v:Int | v = if n == 0 then 0 else n + sum (n-1)}
\end{spec}

<br>

**Key Idea**

Calling `sum n` _reveals definition_ at `n` to the type checker. 


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

Calling `sum n` _reveals definition_ at `n` to the type checker. 

Structuring Proofs as Calculations
----------------------------------

<br>

Using combinators from [`ProofCombinators`](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Language/Haskell/Liquid/ProofCombinators.hs#L97-L100)

<br>

\begin{spec}<div/>
(===) :: x:a -> y:{a | y == x} -> {v:a|v == x && v == y}
\end{spec}

_Require_ both sides are equal, _Ensure_ result is equal to both 

Structuring Proofs as Calculations
----------------------------------

<br>

Using combinators from [`ProofCombinators`](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/include/Language/Haskell/Liquid/ProofCombinators.hs#L97-L100)

<br>

\begin{code}
{-@ sum3' :: _ -> { sum 3 = 6 } @-}
sum3' _ =   sum 3
        === 3 + sum 2
        === 3 + 2 + sum 1
        === 3 + 2 + 1 + sum 0
        === 6
\end{code}


Types as Propositions, Programs as Proofs
-----------------------------------------

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
      === 4 + sum 3
      === 4 + 6     ?   sum3 ()   -- lemma { sum 3 == 6 }
      === 10
\end{code}

**`?` is a library operator (read ``because'')**

Adds the _proposition_ `sum3 ()` to the _known facts_ (context)


Types as Propositions, Programs as Proofs
-----------------------------------------

<br>

<p align=center>

| **Code**    |     | **Math**    |
|------------:|:---:|:------------|
| Types       | are | Theorems    |
| Programs    | are | Proofs      |

</p>

Types as Propositions, Programs as Proofs
-----------------------------------------

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

An algorithm to [emulate computation inside SMT logic][popl18]


Proof by Logical Evaluation
---------------------------

<br>

**Long chains of calculations are tedious**

Make the machine do the hard work!

An algorithm to [emulate computation inside SMT logic][popl18]

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
{-@ thm_sum :: n:Nat -> { 2 * sum n = n * (n+1) } @-}

thm_sum 0 = 2 * sum 0
        === 0 * (0+1)
        
thm_sum n = 2 * sum n
        === 2 * (n + sum (n-1))
        === 2 * n + 2 * sum (n-1)
          ? thm_sum (n-1)
        === n * (n+1)
\end{code}

**Exercise:** What happens if we use the wrong induction?


Types as Propositions, Programs as Proofs
-----------------------------------------

<br>

<p align=center>

| **Code**    |     | **Math**    |
|------------:|:---:|:------------|
| Types       | are | Theorems    |
| Programs    | are | Proofs      |
| Functions   | are | Lemmas      |

</p>

Types as Propositions, Programs as Proofs
-----------------------------------------

<br>

<p align=center>

| **Code**    |     | **Math**    |
|------------:|:---:|:------------|
| Types       | are | Theorems    |
| Programs    | are | Proofs      |
| Functions   | are | Lemmas      |
| Branches    | are | Case-Splits |

</p>

Types as Propositions, Programs as Proofs
-----------------------------------------

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
{-@ type AppendAssoc a = xs:[a] -> ys:[a] -> zs:[a] -> 
                          {(xs ++ ys) ++ zs = xs ++ (ys ++ zs)} 
  @-}
\end{code}

Theorems about Data: Associativity of `append`
----------------------------------------------

<br>

Lets write fill in a _calculational_ proof:

\begin{code}
{-@ appendPf :: AppendAssoc a @-}
appendPf xs ys zs = undefined 
\end{code}

**Exercise:** Lets fill the above in!

Types as Propositions, Programs as Proofs
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

Plan
----

<br> 

**Part I:** [Refinements 101](02-refinements.html)

Case Study: [Vector Bounds](03-example-vectors.html)

**Part II:** [Properties of Structures](04-data-properties.html)

Case Study: [Sorting](05-example-sort.html), [Interpreter](06-example-interpreter.html)

**Part III:** [Invariants of Data Structures](07-data-legal.html)

Case Study: [Sorting actually Sorts](08-example-sort.html)

**Part IV:** [Termination](09-termination.html) and [Correctness Proofs](10-reflection.html)

Case Study: **[Optimizing Arithmetic Expressions](11-example-opt.html)**


[curryhoward]: https://en.wikipedia.org/wiki/Curry–Howard_correspondence
[wadler]: http://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-types/propositions-as-types.pdf
[popl18]: https://arxiv.org/abs/1711.03842
