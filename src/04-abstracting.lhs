
<div class="hidden">
\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}

module AbstractingRefinements ( insertSort ) where

import Prelude hiding (foldr)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b []     = b
foldr f b (x:xs) = f x (foldr f b xs)

\end{code}
</div>

Invariants In Constructors
--------------------------

<div class="mybreak"><br></div>

Many *many* possibilities ...

Invariants In Constructors
--------------------------

<div class="mybreak"><br></div>

Lets specify **ordered lists**:

<div class="hidden">
\begin{code}
data OList a = Emp
             | (:<) { hd :: a
                    , tl :: OList a }
infixr 9 :<
\end{code}
</div>

\begin{code}
{-@ data OList a = Emp
                 | (:<) { hd :: a
                        , tl :: OList {v:a | hd <= v}}
  @-}
\end{code}

Invariants In Constructors
--------------------------

<div class="mybreak"><br></div>

**Make illegal values unrepresentable!**

\begin{code}
ok :: OList Int
ok = 1 :< 2 :< 3 :< Emp

bad :: OList Int
bad = 1 :< 3 :< 2 :< Emp
\end{code}

Invariants In Constructors
--------------------------

<div class="mybreak"><br></div>

Make illegal values unrepresentable!

<div class="mybreak"><br></div>

But its tedious to have **multiple list types** ...

Abstracting Refinements
-----------------------

<div class="mybreak"><br></div>

**Parameterize types over refinements!** [[ESOP 13]](esop13)

Abstracting Refinements
-----------------------

<div class="mybreak"><br></div>

**Parameterize types over refinements!** [[ESOP 13]](esop13)

<div class="mybreak"><br></div>

\begin{spec}
data [a]<p :: a -> a -> Prop> where
  = []
  | (:) { hd :: a
        , tl :: [{v:a | p hd v}]}
\end{spec}

Abstracting Refinements
-----------------------

<div class="mybreak"><br></div>

**Instantiate** refinements to get different invariants!

<div class="mybreak"><br></div>

\begin{code}
{-@ type Incrs a = [a]<{\x y -> x <= y}> @-}
{-@ type Decrs a = [a]<{\x y -> x >= y}> @-}
{-@ type Diffs a = [a]<{\x y -> x /= y}> @-}
\end{code}


Using Abstract Refinements
--------------------------

<div class="mybreak"><br></div>

**Inference FTW!**

<div class="mybreak"><br></div>

\begin{code}
{-@ insertSort :: (Ord a) => [a] -> Incrs a @-}
insertSort xs = foldr insert [] xs

insert x []     = [x]
insert x (y:ys)
  | x < y       = x : y : ys
  | otherwise   = y : insert x ys
\end{code}

Recap
-----

<div class="mybreak"><br></div>

**Abstract Refinements**

Parametric Polymorphism for Refinement Types

Recap
-----

<div class="mybreak"><br></div>

**Abstract Refinements**

Parametric Polymorphism for Refinement Types

<div class="mybreak"><br></div>

**Bounded Refinements**

Bounded Quantification for Refinement Types

[[IFCP 2015]](icfp15)


Bounded Refinements
-------------------

<div class="mybreak"><br></div>

The **bound** `Inductive` says relation `step` preserves `inv`

\begin{spec}
Inductive inv step = \y ys acc v ->
  inv ys acc ==> step y acc v ==> inv (y:ys) v
\end{spec}

Bounded Refinements
-------------------

<div class="mybreak"><br></div>

The **bound** `Inductive` says relation `step` preserves `inv`

\begin{spec}
Inductive inv step = \y ys acc v ->
  inv ys acc ==> step y acc v ==> inv (y:ys) v
\end{spec}

**Key Idea**

Bound is a Horn Clause over (Abstract) Refinements



Bounded Refinements
-------------------

<div class="mybreak"><br></div>

The type of `foldr` encodes **induction over lists** ...

\begin{spec}
foldr :: (Inductive inv step)
      => (x:a -> acc:b -> b<step x acc>)
      -> b<inv []>
      -> xs:[a]
      -> b<inv xs>
\end{spec}

Bounded Refinements
-------------------

<div class="mybreak"><br></div>

... and lets us verify:


\begin{spec}
insertSort :: (Ord a)
           => xs:[a]
           -> {v:Incrs a | elts v == elts xs}

insertSort = foldr insert []
\end{spec}

[[continue]](05-concl.html)

[esop13]: https://ranjitjhala.github.io/static/abstract_refinement_types.pdf
[icfp15]: https://ranjitjhala.github.io/static/bounded_refinement_types.pdf
