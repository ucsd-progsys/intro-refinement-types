Structural Induction
--------------------

<br>

<br>

How we *express* and *prove* properties on data types?
<br>

<br>

\begin{code}
{-# LANGUAGE TupleSections    #-}
module StructuralInduction where

import Prelude hiding (map, id, length, return)
import Language.Haskell.Liquid.ProofCombinators
\end{code}



The list data type
--------------------

<br>
A user defined list,
<br>

\begin{code}
data L a = N | C a (L a)

{-@ data L [length] @-}
\end{code}

with its anchored size function.
<br>

\begin{code}
{-@ measure length @-}
{-@ length :: L a -> Nat @-}
length N        = 0
length (C _ xs) = 1 + length xs
\end{code}



Definition of Structural Inductive Functions
---------------------------------------------

<br>

\begin{code}
{-@ reflect map @-}
map :: (a -> b) -> L a -> L b
map f N        = N
map f (C x xs) = f x `C` map f xs
\end{code}


Definition of Non Recursive Functions
-------------------------------------

<br>
Non-recursive functions reflect too!
<br>

\begin{code}
{-@ reflect id @-}
id :: a -> a
id x = x

{-@ reflect compose @-}
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
\end{code}


Proving Map-Identity
----------------------

<br>
Optimization property: to `map` identity do not transverse the list!
<br>


\begin{code}
{-@ mapId :: xs:L a -> { map id xs == id xs } @-}
mapId N
  =   map id N
  ==. N
  ==. id N
  *** QED
mapId (C x xs)
  =   map id (C x xs)
  ==. id x `C` map id xs
  ==. x `C` map id xs
  ==. x `C` id xs         ? mapId xs
  ==. x `C` xs
  ==. id (x `C` xs)
  *** QED
\end{code}

Proof by case splitting and recursive call.


Automation: Proving Map-Identity
----------------------

<br>
Pretty Verbose Proof: Proof Automation!
<br>

\begin{code}
{-@ LIQUID "--automatic-instances=liquidinstanceslocal" @-}
\end{code}

\begin{code}
{-@ automatic-instances mapIdAuto @-}

{-@ mapIdAuto :: xs:L a -> { map id xs == id xs } @-}
mapIdAuto N        = trivial
mapIdAuto (C x xs) = mapIdAuto xs
\end{code}

Proof Generation:

  - Automatic Unfolding, but
  - Manual case splitting.



Proving Map-Fusion
--------------------------

<br>
Optimization property: transverse the list only once!
<br>


\begin{code}
{-@ automatic-instances mapFusion @-}
{-@ mapFusion :: f:(b -> c) -> g:(a -> b) -> xs:L a
  -> { map  (compose f g) xs == (map f) (map g xs) } @-}
mapFusion f g xs = undefined "Prove me!"
\end{code}

**Exercise:** Can you prove map-fusion?



Onto Monoid Laws
----------------
<br>

Reflect the monoid list operators

<br>

\begin{code}
{-@ reflect append @-}
append :: L a -> L a -> L a
append N ys        = ys
append (C x xs) ys = x `C` append xs ys

{-@ reflect empty @-}
empty :: L a
empty = N
\end{code}

Monoid Laws: Left Identity
--------------------------

<br>
Lets prove the left identity monoid law!
<br>

\begin{code}
{-@ automatic-instances emptyLeft @-}

{-@ emptyLeft :: x:L a -> { append empty x == x }  @-}
emptyLeft x = undefined "Prove me!"
\end{code}




Monoid Laws: Right Identity
---------------------------------------------
<br>
Lets prove the right identity monoid law!
<br>

\begin{code}
{-@ automatic-instances emptyRight @-}
{-@ emptyRight :: x:L a -> { append x empty == x }  @-}
emptyRight _ = undefined "Prove me!"
\end{code}

Monoid Laws: Associativity
---------------------------------------------

<br>
Lets prove the associativity monoid law!
<br>

\begin{code}
{-@ automatic-instances appendAssoc @-}

{-@ appendAssoc :: xs:L a -> ys:L a -> zs:L a
  -> {append xs (append ys zs) == append (append xs ys) zs } @-}
appendAssoc _ _ _          = undefined "Prove me!"
\end{code}


Onto Monad Laws!
----------------

<br>
Define monad list operators
<br>

\begin{code}
{-@ reflect return @-}
return :: a -> L a
return x = C x N

{-@ reflect bind @-}
bind :: L a -> (a -> L b) -> L b
bind N _ = N
bind (C x xs) f = append (f x) (bind xs f)
\end{code}

Monad Laws: Left Identity
---------------------------------------------
<br>
Lets prove the left identity monad law!
<br>

\begin{code}
{-@ automatic-instances leftIdentity @-}

{-@ leftIdentity :: x:a -> f:(a -> L b)
  -> { bind (return x) f == f x } @-}
leftIdentity x f = undefined "Prove me!"
\end{code}

Monad Laws: Right Identity
---------------------------------------------
<br>
Lets prove the right identity monad law!
<br>

\begin{code}
{-@ automatic-instances rightIdentity @-}

{-@ rightIdentity :: x:L a -> { bind x return == x } @-}
rightIdentity _ = undefined "Prove me!"
\end{code}


Monad Laws: Associativity
---------------------------------------------
<br>
To prove associativity, lets assume a helper lemma!
<br>


- Bind distribution

\begin{code}
{-@ automatic-instances bindAppend @-}
{-@ bindAppend :: xs:L a -> ys:L a -> f:(a -> L b)
     -> { bind (append xs ys) f == append (bind xs f) (bind ys f) } @-}
bindAppend N _ _
  = trivial
bindAppend (C x xs) ys f
  = appendAssoc (f x) (bind xs f) (bind ys f)
  &&& bindAppend xs ys f
\end{code}


Monad Laws: Associativity
---------------------------------------------
<br>
Lets prove the associativity monad law!
<br>

\begin{code}
{-@ automatic-instances associativity @-}
{-@ associativity :: m:L a -> f: (a -> L b) -> g:(b -> L c)
  -> {bind (bind m f) g == bind m (\x:a -> (bind (f x) g)) } @-}
associativity m f g = undefined "Prove me!"
\end{code}


Prove fancy lists properties
--------------------------------

- Functor Laws
    - Identity:     `map id == id`
    - Distribution: `map (compose f g) == compose (map f) (map g)`

<br>

- Monoid Laws
    - Left Identity: `append empty x == x`
    - Right Identity: `append x empty == x`
    - Associativity: `append xs (append ys zs) == append (append xs ys) zs`

<br>

- Monad Laws
    - Left Identity: `bind (return x) f == f x`
    - Right Identity: `bind x return == x`
    - Associativity: `bind (bind m f) g == bind m (\x:a -> (bind (f x) g))`


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
| **Structural Induction:**| Theorems about Data Types  |

<br>
<br>

<div class="fragment">
**Next:** [Case Study: MapReduce](08-case-study-map-reduce.html): Program Properties that matter!
</div>




Appendix
---------

\begin{code}
{-@ LIQUID "--betaequivalence" @-}
{-@ LIQUID "--higherorder"     @-}
{-@ LIQUID "--exact-data-cons" @-}

length :: L a -> Int
mapFusion :: (b -> c) -> (a -> b) -> L a -> Proof
mapId :: L a -> Proof
mapIdAuto :: L a -> Proof
emptyLeft :: L a -> Proof
emptyRight :: L a -> Proof
appendAssoc :: L a -> L a -> L a -> Proof
leftIdentity :: a -> (a -> L b) -> Proof
rightIdentity :: L a -> Proof
associativity :: L a -> (a -> L b) -> (b -> L c) -> Proof
\end{code}
