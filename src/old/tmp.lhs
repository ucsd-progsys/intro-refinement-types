
\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}

module IntroToRefinementTypes
       ( sum
       , sumBad
       , sumInfer
       , sumHO
       , average
       , insertSort
       )
       where

import Prelude hiding (foldr, map, sum, length, (!))
import qualified Data.Vector as V
\end{code}


<div class="slide">

<br>
<br>
<br>
<br>

<h1 style="border-bottom:none">An Introduction to Refinement Types</h1>

<h4 style="border-bottom:none"><i>Ranjit Jhala (UCSD)</i></h4>

</div>


The First *Bug*
---------------

<img src="img/firstbug-crop2.jpg" height=300px/>

**Page from Harvard Mark II log**

A dead moth removed from the device

Fast forward to Present Day
---------------------------

<img src="img/news-bug-1.png" height=300px>


Fast forward to Present Day
---------------------------

<img src="img/news-bug-2.png" height=300px>


Fast forward to Present Day
---------------------------

<img src="img/news-bug-3.png" height=300px>


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Specifications %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


Preconditions
-------------

\begin{code}
{-@ at :: v:V.Vector a -> {i:Nat | i < vlen v} -> a @-}
at :: V.Vector a -> Int -> a
at v i = undefined
\end{code}

Postconditions
--------------

\begin{code}
{-@ size :: v:V.Vector a -> {n:Int | n == vlen v} @-}
size :: V.Vector a -> Int
size = undefined
\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Verification **%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


Refinement Checking (with Error)
--------------------------------

\begin{code}
sumBad :: V.Vector Int -> Int
sumBad v = loop 0
  where
    {-@ loop :: Nat -> Int @-}
    loop i
      | i <= size v = at v i + loop (i + 1)
      | otherwise   = 0
\end{code}


Refinement Checking (Safe)
-------------------

\begin{code}
sum :: V.Vector Int -> Int
sum v = loop 0
  where
    {-@ loop :: Nat -> Int @-}
    loop i
      | i < size v = at v i + loop (i + 1)
      | otherwise  = 0
\end{code}


Refinement Checking (How)
-------------------------

TODO: Show VCs


Refinement Inference
--------------------

\begin{code}
sumInfer :: V.Vector Int -> Int
sumInfer v = loop 0
  where
    {-@ loop :: _ -> _ @-}
    loop i
      | i < size v = at v i + loop (i + 1)
      | otherwise  = 0
\end{code}

Refinement Inference (How)
-------------------------

TODO: Show Horn-Clauses


Collections & Higher-Order Functions
------------------------------------

\begin{code}
range :: Int -> Int -> [Int]
range lo hi
  | lo < hi   = lo : range (lo + 1) hi
  | otherwise = []

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b []     = b
foldr f b (x:xs) = f x (foldr f b xs)

sumHO :: V.Vector Int -> Int
sumHO v = foldr add 0 is
  where
    add i n = n + at v i
    is  = range 0 (size v)
\end{code}

Preconditions on Structures
---------------------------

Oops, only valid on **non-empty** lists!

\begin{code}

{-@ average :: ListNE Int -> Int @-}
average xs = total `div` n
  where
    total  = foldr (+) 0 xs
    n      = length xs

length        :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs
\end{code}

Need to describe *non-emptiness* !

Measuring Structures
--------------------

\begin{code}
{-@ measure length @-}
{-@ type ListNE a = {v:[a] | 0 < length v } @-}
\end{code}

Now lets go back and *fix* `average` ...

Measures are Refined Constructors
---------------------------------

\begin{spec}
data [a] where
  []  :: {v:[a] | length v == 0}

  (:) :: a
      -> t:[a]
      -> {v:[a] | length v == 1 + length t}
\end{spec}

`length` is **uninterpreted** in Refinement Logic

$$\forall x, y. x = y \Rightarrow f(x) = f(y)$$


Using Measures
--------------

Recall the `map` function:

\begin{code}
{-@ map :: (a -> b) -> [a] -> [b] @-}
map f []     = []
map f (x:xs) = f x : map f xs
\end{code}

What's the problem here? (Lets fix it!)

\begin{code}
{-@ hwAverage :: ListNE (a, Int) -> Int @-}
hwAverage nxs = average (map snd nxs)

{- type ListN a N = {v:[a] | length v == N} @-}
{- type ListX a X = ListN a (length X)      @-}
\end{code}

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
\end{code}

But its tedious to use *separate* lists...


Abstracting Refinements
-----------------------

**Parameterize** (specifications) with refinements!

\begin{spec}
data [a]<p :: a -> a -> Prop>
      = []
      | (:) { hd :: a
            , tl :: [{v:a | p hd v}]}
\end{spec}

Abstracting Refinements
-----------------------

**Instantiate** refinements to get different invariants!

\begin{code}
{-@ type Incrs a = [a]<{\x y -> x <= y}> @-}
{-@ type Decrs a = [a]<{\x y -> x >= y}> @-}
{-@ type Diffs a = [a]<{\x y -> x /= y}> @-}

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

Encode _induction_ as the type of `foldr`

\begin{spec}
foldr :: (Inductive inv step)
      => (x:a -> acc:b -> b<step x acc>)
      -> b<inv []>
      -> xs:[a]
      -> b<inv xs>
\end{spec}

Which lets us automatically verify,

\begin{spec}
insertSort :: (Ord a) => xs:[a] -> {v:Incrs a | size v == size xs}
\end{spec}
