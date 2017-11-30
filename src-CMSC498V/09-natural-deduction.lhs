Programs are Proofs
------------------

From <a href="http://goto.ucsd.edu/~nvazou/presentations/CMSC498V/06-reflection.html"> previous</a> lecture:

\begin{spec}
fibMono :: n:Nat -> m:{Nat | n < m }  -> {fib n <= fib m}
fibMono     = fMono fib fibUp
\end{spec}

Can we prove any (higher order) logical property?



Logical Properties
-------------------

<br>

Like programming languages, logic has

- **syntax** (e,∧,∨,¬,⇒,∀,∃),
- **meaning** (If φ and ψ hold, then φ∧ψ holds), and
- **"evaluation"** (decision procedures).


Natural Deduction ...
-------------------

<br>
... is a decision procedure for logic. 

<br>

<a href="https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence#Natural_deduction_and_lambda_calculus">Curry Howard Correspondence</a>

Propositions <~> Refinement Types

Natural Deduction <~> Lambda Calculus (here Haskell)

<div class="hidden">

This is a Haskell Module!
---------------------------

\begin{code}
module NaturalDeduction where 
import Language.Haskell.Liquid.ProofCombinators

import Prelude hiding ((++), length)
{-@ infix   ++ @-}
\end{code}

</div>

Motivation
-----------

<img src="img/curry-howard.png" height=250px>

Propositions <~> Refinement Types
------------------------------------
<img src="img/encoding0.png" height=200px>

Logic as Types: Native Terms
----------------------------

`e` <~> `{v:() | e}`

\begin{code}
{-@ fact1 :: {v:() | 2 == 1 + 1 } @-}
fact1 = ()
\end{code}

\begin{code}
{-@ fact2 :: {v:() | 2 /= 3 } @-}
fact2 = ()
\end{code}

Propositions <~> Refinement Types
------------------------------------
<img src="img/encoding1.png" height=200px>


Logic as Types: Conjunction as Pairs
----------------------------

`ϕ1 ∧ ϕ2` <~> `(ϕ1,ϕ2)`

\begin{code}
{-@ conj :: ϕ1:{Bool | ϕ1 } -> ϕ2:{Bool | ϕ2 } 
         -> {v:(Bool, Bool) | ϕ1 && ϕ2} @-}
conj ϕ1 ϕ2 = (ϕ1,ϕ2)
\end{code}

If you know ϕ1 and ϕ2 hold...

then (ϕ1,ϕ2) is a λ-term that shows ϕ1 ∧ ϕ2 holds.


Typing Pairs
------------

<br>
<img src="img/pairsLeft.png" height=80px>

<br>
**Q:** How do we type right?

Typing Pairs
------------

<img src="img/pairsTypes.png" height=280px>

Conjunction <~> Pairs 
------------

<img src="img/pairsCorrespondence.png"  height=280px>

Natural Deduction for Conjunction  
------------

<img src="img/pairsNatDed.png"  height=280px >

Conjunction Example: Natural Deduction
----------------------------

<img src="img/conjEx1.png"  height=200px >


Conjunction Example: Isomorphism 
----------------------------

<br>
<img src="img/conjEx2.png"  height=150px >


Conjunction Example in Practise
----------------------------


Let's prove: 
`ϕconj ≡ ϕ1, φ2 ∧ ϕ3 |- φ1 ∧ ϕ3`


<br>
\begin{code}
{-@ ex1 :: φ1:Bool -> φ2:Bool -> φ3:Bool 
    -> {v:() | φ1}
    -> ({v:() | φ2}, {v:() | φ3})
    -> ({v:() | φ1}, {v:() | φ3}) @-}
ex1 _ _ _ x1 x23 = undefined
\end{code}

**Q:** Can you prove `ϕconj`?

Propositions <~> Refinement Types
------------------------------------
<img src="img/encoding2.png" height=200px>


Logic as Types: Disjunction as Either
----------------------------

`ϕ1 ∨ ϕ2` <~> `Either ϕ1 ϕ2`

where `data Either a b = Left a | Right b`.

\begin{code}
{-@ disj :: φ1:Bool -> φ2:Bool 
         -> {v:() | φ1 || φ2 } -> {v:Either () () | φ1 || φ2} @-}
disj φ1 φ2 p | φ1 = Left  p
             | φ2 = Right p
\end{code}

If you know ϕ1 or ϕ2 holds...

then `Left ϕ1` and `Right ϕ2` are λ-terms that shows ϕ1 ∨ ϕ2 holds.


Rules for Disjunction
----------------------

<br>
<img src="img/disjunction.png"  height=200px >


Propositions <~> Refinement Types
------------------------------------
<img src="img/encoding3.png" height=200px>


Logic as Types: Implication as Function
----------------------------
<br>
<img src="img/implication.png"  height=200px >


Modus Ponens ...
-------------

<br>
... is another name for the Implication Elimination Rule.


<img src="img/modusPonens.png" height=150px>


Propositions <~> Refinement Types
------------------------------------
<img src="img/encoding4.png" height=200px>

Logic as Types: Negation as Impication
----------------------------

Negetion is just implication to False!

<img src="img/negetion.png" height=200px>



Propositions <~> Refinement Types
------------------------------------
<img src="img/encoding5.png" height=200px>


Logic as Types: Forall as Function
----------------------------
<img src="img/forall.png" height=200px>

Propositions <~> Refinement Types
------------------------------------
<img src="img/encoding6.png" height=200px>

Logic as Types: Exists as Pair
----------------------------
<img src="img/exists.png" height=200px>

Propositions <~> Refinement Types
------------------------------------
<img src="img/encoding.png" height=200px>



**Q:** Can you encode the follοwing? 
`ϕ ≡ (∃x.∀y.(p x y)) ⇒ (∀y.∃x.(p x y))`


Example existsAll Encoded 
------------------

\begin{code}
{-@ exAll :: p:(a -> a -> Bool)
          -> (x::a, y:a -> {v:() | p x y}) 
          -> y:a 
          -> (x::a, {v:() | p x y}) @-}
exAll p = undefined
\end{code}


**Q:** Can you define `exAll`?

Example existsAll in Natural Deduction 
---------------------------------------
<br>

<img src="img/exAll.png" height=200px>


Example II: Distributing Qualifiers
-------------------------------------
<img src="img/encoding.png" height=100px>

Let's prove: 
`ϕ∃ ≡ (∃x.p x ∨ q x) ⇒ ((∃x.p x) ∨ (∃x.q x))`


\begin{code}
{- exDistOr :: p : _ -> q : _  @-}
exDistOr = undefined 
\end{code}

Example III: Distributing Qualifiers
-------------------------------------
<img src="img/encoding.png" height=100px>

Let's prove: 
`ϕ∀ ≡ (∀x.p x ∧ q x) ⇒ ((∀x.p x) ∧ (∀x.q x))`


\begin{code}
{- allDistAnd :: p : _ -> q : _  @-}
allDistAnd = undefined 
\end{code}



Example IV: Lists
-------------------------------------
<img src="img/encoding.png" height=100px>

Let's prove: 
`∀xs.((∃ys. xs = ys ++ ys) ⇒ (∃n.length xs = n + n))`

\begin{code}
{- evenLen :: xs:_   @-}
evenLen = undefined 
\end{code}

*Hint:* You can use 
\begin{code}
{-@ lenAppend :: xs : L a -> ys : L a 
              -> { length ( xs ++ ys ) = length xs + length ys } @-}
\end{code}

Example V: Natural Induction
-------------------------------------
<img src="img/encoding.png" height=100px>

Let's prove: 
`ϕind ≡ (p 0 ∧ (∀n.p (n − 1) ⇒ p n) ⇒ ∀n.p n)`

\begin{code}
ind = undefined
\end{code}


Summing up
-------------------------------------

Natural Deduction Proofs <~> Lambda Calculus (here Haskell)

Propositions <~> Refinement Types

<img src="img/encoding.png" height=200px>


**Further Reading:**

<a href="http://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-types/propositions-as-types.pdf"> Propositions as Types, *by Wadler* </a>
&
<a href="https://nikivazou.github.io/static/popl18/refinement-reflection.pdf"> Refinement Reflection, *by me et al.* </a>



List Helpers
------------
\begin{code}
{-@ LIQUID "--higherorder"    @-}
{-@ LIQUID "--exact-data-con" @-}

{-@ LIQUID "--automatic-instances=liquidinstances" @-}
{-@ lenAppend :: xs : L a -> ys : L a 
              -> { length ( xs ++ ys ) = length xs + length ys } @-}
lenAppend :: L a -> L a -> ()
lenAppend N        _  = () 
lenAppend (C x xs) ys = lenAppend xs ys

data L a = N | C a (L a)
{-@ data L [length] a = N | C {hd :: a, tl :: L a} @-}

length :: L a -> Int 
{-@ length :: x:L a -> {v:Nat | v == length x} @-}
{-@ measure length @-}
length N        = 0 
length (C _ xs) = 1 + length xs 

{-@ reflect ++ @-}
N        ++ ys = ys 
(C x xs) ++ ys = C x (xs ++ ys)
\end{code}

Appendix
----

\begin{code}
fact1 :: ()
fact2 :: ()
conj  :: Bool -> Bool -> (Bool,Bool) 
disj  :: Bool -> Bool -> () -> Either () ()
ex1   :: Bool -> Bool -> Bool -> () -> ((),()) -> ((),())
exAll :: (a -> a -> Bool) -> (a, a -> ()) -> a -> (a, ()) 
exDistOr :: (a -> Bool) -> (a -> Bool) -> (a, Either c d) -> Either (a, c) (a,d)
allDistAnd :: (a -> Bool) -> (a -> Bool) -> (a -> (b,c)) -> (a -> b, a -> c)
ind :: (Int -> Bool) -> ((), Int -> () -> ()) -> Int -> () 
evenLen :: L a -> (L a, Proof) -> (Int, Proof)
\end{code}