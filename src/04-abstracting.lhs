
<div class="hidden">

\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}

module AbstractingRefinements ( insertSort ) where

import Prelude hiding (foldr, map, sum, length, (!))
\end{code}
</div>

Invariants In Constructors
--------------------------

Many possibilities -- lets *require* increasing order:

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

Make illegal states unrepresentable!

\begin{code}
ok :: OList Int
ok = 1 :< 2 :< 3 :< Emp

bad :: OList Int
bad = 1 :< 3 :< 2 :< Emp
\end{code}

But its tedious to use *separate* "lists" ...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Abstracting Refinements %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Abstracting Refinements
-----------------------

**Parameterize** (specifications) with refinements!

\begin{spec}
data [a]<p :: a -> a -> Prop> where
  = []
  | (:) { hd :: a
        , tl :: [{v:a | p hd v}]}
\end{spec}

Abstracting Refinements
-----------------------

**Instantiate** refinements to get different invariants!

\begin{code}
{-@ type Incrs a = [a]<{\x y -> x < y}> @-}
{-@ type Decrs a = [a]<{\x y -> x > y}> @-}
{-@ type Diffs a = [a]<{\x y -> x /= y}> @-}
\end{code}

\begin{code}
{-@ ups   :: Incrs Integer @-}
ups       = [1, 2, 3, 4]

{-@ downs :: Decrs Integer @-}
downs     = [4, 3, 2, 1]

{-@ diffs :: Diffs Integer @-}
diffs     = [1, 3, 2, 4]
\end{code}


Using Abstract Refinements
--------------------------

\begin{code}
{-@ insertSort :: (Ord a) => [a] -> Incrs a @-}
insertSort xs = foldr insert [] xs

insert x []     = [x]
insert x (y:ys)
  | x < y       = x : y : ys
  | otherwise   = y : insert x ys
\end{code}


Bounded Refinements
-------------------

**Bounded Quantification** for Abstract Refinements

e.g. Below says relation `step` is preserves `inv`:

\begin{spec}
Inductive inv step = \y ys acc v ->
  inv ys acc ==> step y acc v ==> inv (y:ys) v
\end{spec}

Bounded Refinements
-------------------

Example: **Induction** as the type of `foldr`

\begin{spec}
foldr :: (Inductive inv step)
      => (x:a -> acc:b -> b<step x acc>)
      -> b<inv []>
      -> xs:[a]
      -> b<inv xs>
\end{spec}

Which lets us verify,

\begin{spec}
insertSort :: (Ord a) => xs:[a] -> {v:Incrs a | size v == size xs}
insertSort = foldr insert []
\end{spec}
