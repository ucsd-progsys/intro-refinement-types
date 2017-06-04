
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Specifications %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


Preconditions
-------------

\begin{code}
{-@ at :: v:V.Vector a -> {i:Nat | i < vlen v} -> a @-}
at :: V.Vector a -> Int -> a
at v i = (V.!)
\end{code}

Postconditions
--------------

\begin{code}
{-@ size :: v:V.Vector a -> {n:Int | n == vlen v} @-}
size :: V.Vector a -> Int
size = V.length
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

Verification Conditions

$$\begin{array}{lll}
0 \leq i \wedge n = \mathit{vlen}\ v \wedge i < n
  & \Rightarrow v = i
  & \Rightarrow 0 \leq v < vlen v
  & \mbox{(\mathtt{at\ v\ i})} \\
  & & & \\
\True
  & \Rightarrow v = 0
  & \Rightarrow 0 \leq v
  & \mbox{(\mathtt{loop}\ 0)} \\
  & & & \\
0 \leq i \wedge n = \mathit{vlen}\ v \wedge i < n
  & \Rightarrow v = i + 1
  & \Rightarrow 0 <= v \\
  & \mbox{(\mathtt{loop}\ \mathtt{i+1})} \\
\end{array}$$

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inference %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data/Hofs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

sumHO :: Vector Int -> Int
sumHO v = foldr add 0 is
  where
    add = \i n -> n + at v i
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
