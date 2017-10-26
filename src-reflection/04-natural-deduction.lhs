Encoding Natural Deduction
------------------------------------

<br>

Let's see how natural deduction is encoded as type derivation!

\begin{code}
module NaturalDeduction where

{-@ LIQUID "--higherorder"    @-}
{-@ LIQUID "--exact-data-con" @-}

import Language.Haskell.Liquid.ProofCombinators

import Prelude hiding ((++), length)
{-@ infix   ++ @-}
\end{code}


Propositions as Types 
----------------------


<br>
<center>
<img src="img/props-as-types.png" height=250px>

</center>


Natural Deduction as Type Derivation
------------------------------------

<br>

For example, the or-elimination rule
<center>
<img src="img/or-nat.png" height=100px>
</center>

becomes

<center>
<img src="img/or-type.png" height=100px>
</center>

Exists over Forall in Natural Deduction 
------------------------------------

<br>
<center>
`ϕ ≡ (∃x.∀y.(p x y)) ⇒ (∀y.∃x.(p x y))`

<img src="http://goto.ucsd.edu/~nvazou/images/reflect-figure4.png" height="200" align="middle" />
</center>

Read bottom up, it is a proof!

Exists over Forall in Natural Deduction 
------------------------------------

<br>
<center>
`ϕ ≡ (∃x.∀y.(p x y)) ⇒ (∀y.∃x.(p x y))`

`λe y.case e of {(x, ex ) → (x, ex y)}`

<img src="http://goto.ucsd.edu/~nvazou/images/reflect-figure4.png" height="200" align="middle" />
</center>

Read top down, we get a proof term!


Exists over Forall in Haskell!
------------------------------------

<br>
<center>
`ϕ ≡ (∃x.∀y.(p x y)) ⇒ (∀y.∃x.(p x y))`

`λe y.case e of {(x, ex ) → (x, ex y)}`
</center>

<br>

\begin{code}
{-@ exAll :: p:(a -> a -> Bool)
          -> (x::a, y:a -> {v:Proof | p x y}) 
          -> y:a 
          -> (x::a, {v:Proof | p x y}) @-}
exAll p = \e y -> case e of {(x, ex) -> (x, ex y)}
\end{code}



Distributing Existentials 
------------------------

<br>
<center>
`ϕ∃ ≡ (∃x.p x ∨ q x) ⇒ ((∃x.p x) ∨ (∃x.q x))`
</center>
<br>

\begin{code}
{-@ exDistOr :: p : _ -> q : _
             -> (x :: a ,Either {v:b | p x } {v:c | q x })
             -> Either (x::a , {v:b | p x }) (x::a , {v:c | q x }) @-}
exDistOr _ _ (x, Left  px) = Left  (x, px)
exDistOr _ _ (x, Right qx) = Right (x, qx)
\end{code}

Distributing Universals 
------------------------

<br>
<center>
`ϕ∀ ≡ (∀x.p x ∧ q x) ⇒ ((∀x.p x) ∧ (∀x.q x))`
</center>
<br>
\begin{code}
{-@  allDistAnd :: p:_ -> q:_ 
               -> (x:a -> ({v:Bool | p x }, {v:Bool| q x}))
               -> ((x:a -> {v:Bool | p x }), (x:a -> {v:Bool| q x })) @-}
allDistAnd _ _ andx = 
    ((\x -> case andx x of (px, _ ) -> px)
    ,(\x -> case andx x of (_ , qx) -> qx))
\end{code}

Let's use SMT to simplify the proof!


Properties of User Defined Datatypes 
------------------------------------

<br>
<center>
`ϕ ≡ ∀xs.((∃ys. xs = ys ++ ys) ⇒ (∃n.length xs = n + n))`
</center>
<br>

\begin{code}
{-@ evenLen :: xs:_ 
            -> (ys::L a, {v:Proof |  xs = ys ++ ys }) 
            -> (n ::Int, {v:Proof | length xs = n + n }) @-}
evenLen xs (ys, pf) = (length ys, lenAppend ys ys &&& pf )
\end{code}

where
\begin{code}
{-@ lenAppend :: xs:_ -> ys:_ 
              -> { length (xs ++ ys) = length xs + length ys } @-}
\end{code}





Induction on Natural Numbers
----------------------------

<br>
<center>
`ϕind ≡ (p 0 ∧ (∀n.p (n − 1) ⇒ p n) ⇒ ∀n.p n)`
</center>
<br>

\begin{code}
{-@ ind :: p:_ 
  -> ({v:Proof| p 0}, (n:Nat -> {v:Proof | p (n-1)} -> {v:Proof | p n})) 
  -> n:Nat 
  -> {v:Proof | p n} @-}
ind p (p0, pn) n 
  | n == 0    = p0
  | otherwise = pn n (ind p (p0, pn) (n-1))
\end{code}



Summary: 
---------

<br>

- Refinement Reflection and Proof by Logical Evaluation combined ...

- ... allow for complete verification with SMT-automation!

<br>

- **Case Study:** MapReduce Equivalence
- **Case Study:** Encoding Natural Deduction


Haskell Sigs
-------------

\begin{code}
exDistOr :: (a -> Bool) -> (a -> Bool) -> (a, Either c d) -> Either (a, c) (a,d)
exAll :: (a -> a -> Bool) -> (a, a -> Proof) -> a -> (a, Proof) 
ind :: (Int -> Bool) -> (Proof, Int -> Proof -> Proof) -> Int -> Proof 
allDistAnd :: (a -> Bool) -> (a -> Bool) -> (a -> (Bool,Bool)) 
           -> (a -> Bool, a -> Bool)
evenLen :: L a -> (L a, Proof) -> (Int, Proof)
\end{code}


Code for Lists
---------------

\begin{code}
{-@ LIQUID "--automatic-instances=liquidinstances" @-}
lenAppend :: L a -> L a -> Proof
lenAppend N        _  = trivial 
lenAppend (C x xs) ys = lenAppend xs ys
\end{code}

\begin{code}
data L a = N | C a (L a)
{-@ data L [length] a = N | C {hd :: a, tl :: L a} @-}

length :: L a -> Int 
{-@ length :: x:L a -> {v:Nat | v == length x} @-}
{-@ measure length @-}
length N        = 0 
length (C _ xs) = 1 + length xs 
\end{code}

The appending function `(++)`
is Haskell's usual append reflected in the logic.
\begin{code}
{-@ reflect ++ @-}
N        ++ ys = ys 
(C x xs) ++ ys = C x (xs ++ ys)
\end{code}
