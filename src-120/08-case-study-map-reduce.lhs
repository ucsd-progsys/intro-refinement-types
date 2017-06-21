<div class="hidden">

\begin{code}
{-# LANGUAGE TupleSections    #-}

{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--higherorder"    @-}
{-@ LIQUID "--totality"       @-}
{-@ LIQUID "--exactdc"        @-}
{-@ LIQUID "--automatic-instances=liquidinstanceslocal" @-}

module MapReduce where

import Prelude hiding (mconcat, map, split, take, drop, sum, (++))
import Language.Haskell.Liquid.ProofCombinators

map :: (a -> b) -> List a -> List b
sumEq :: Int -> List Int -> Proof

sumRightId :: List Int -> Proof
sumDistr :: List Int -> List Int -> Proof

mRTheorem :: Int -> (List a -> b) -> (b -> b -> b)
          -> (List a -> Proof)
          -> (List a -> List a -> Proof)
          -> List a -> Proof

appendTakeDrop :: Int -> List a -> Proof

llen :: List a -> Int
{-@ infix ++ @-}
(++) :: List a -> List a -> List a
drop :: Int -> List a -> List a
take :: Int -> List a -> List a
\end{code}

</div>




Case Study: MapReduce
=====================
<br>
Chunk input, map operation (in parallel), and reduce the results.
<br>
<br>

<p align=center>
<img src="img/map-reduce.jpg" height=350px/>
</p>




Implementation
---------------
<br> 
Haskell definition and Reflection
<br>

\begin{code}
{-@ reflect mapReduce @-}
mapReduce :: Int -> (List a -> b) -> (b -> b -> b) -> List a -> b
mapReduce n f op is = reduce op (f N) (map f (chunk n is))

{-@ reflect reduce @-}
reduce :: (a -> a -> a) -> a -> List a -> a
reduce op b N        = b
reduce op b (C x xs) = op x (reduce op b xs)

{-@ reflect map @-}
{-@ map :: (a -> b) -> xs:List a -> {v:List b | llen v == llen xs } @-}
map _  N       = N
map f (C x xs) = f x `C` map f xs

{-@ reflect chunk @-}
chunk :: Int -> List a -> List (List a)
\end{code}


Use Case: Summing List
----------------------

- Standard List Summing
\begin{code}
{-@ reflect sum @-}
sum  :: List Int -> Int
sum N        = 0
sum (C x xs) = x `plus` sum xs
\end{code}

- Reduction Operation
\begin{code}
{-@ reflect plus @-}
plus :: Int -> Int -> Int
plus x y = x + y
\end{code}

- MapReduce List Summing
\begin{code}
{-@ reflect psum @-}
psum :: Int -> List Int -> Int
psum n is = mapReduce n sum plus is
\end{code}

<br>
**Question:** Is `psum` equivalent to `sum`?
<br>



Proving Code Equivalence
-------------------------

- By application of HO Theorem
\begin{code}
{-@ automatic-instances sumEq @-}
{-@ sumEq :: n:Int -> is:List Int -> { sum is == psum n is } @-}
sumEq n is = mRTheorem n sum plus sumRightId sumDistr is
\end{code}

- Distribution of `sum` 
\begin{code}
{-@ automatic-instances sumRightId @-}
{-@ sumDistr :: xs:List Int -> ys:List Int -> {sum (xs ++ ys) == plus (sum xs) (sum ys)} @-}
sumRightId xs = trivial 
\end{code}

- Right Identity of `plus`
\begin{code}
{-@ sumRightId :: xs:List Int -> {plus (sum xs) (sum N) == sum xs} @-}
{-@ automatic-instances sumDistr @-}
sumDistr N ys        = trivial 
sumDistr (C x xs) ys = sumDistr xs ys
\end{code}



Higher Order Map Reduce Theorem
-----------------------
<br>
If `f` is right-id and `op` distributes ...
<br> 
... then it is OK to map-reduce
<br>

\begin{code}
{-@ mRTheorem :: n:Int -> f:(List a -> b) -> op:(b -> b -> b)
     -> rightId:(xs:List a -> {op (f xs) (f N) == f xs } )
     -> distrib:(xs:List a -> ys:List a -> {f (xs ++ ys) == op (f xs) (f ys)} )
     -> is:List a
     -> { mapReduce n f op is == f is }
     / [llen is]
  @-}
\end{code}

<br>
Manual Proof (see Appendix)
<br>

Recap
-----

<br>
<br>

-  **Refinement Reflection:** Allow Haskell functions in Logic
-  <div class="fragment">**Case Study:**</div> Prove Program Equivalence

<br>
<br>

Prove crucial properties **for** Haskell **in** Haskell!

<br>

where Haskell = a general purpose programming language.



Recap
-----

<br>
<br>

1. **Refinements:** Types + Predicates
2. **Measures:** Specify Properties of Data
3. **Case Study:** Sorting
4. **Termination:** Semantic Termination Checking
5. **Reflection:** Allow Haskell functions in Logic!
6. <div class="fragment">**Case Study:**</div> Prove Program Equivalence

<br>
<br>
[Evaluation & Conclusion](file:///Users/niki/intro-refinement-types/dist/_site/01-index.html?slide=27)

Appendix: Proof of `mRTheorem` 
-----------------------------

\begin{code}
mRTheorem n f op rightId _ N
  =   mapReduce n f op N
  ==. reduce op (f N) (map f (chunk n N))
  ==. reduce op (f N) (map f (C N N))
  ==. reduce op (f N) (f N `C` map f N )
  ==. reduce op (f N) (f N `C` N)
  ==. op (f N) (reduce op (f N) N)
  ==. op (f N) (f N)
       ? rightId N
  ==. f N
  *** QED

mRTheorem n f op rightId _ is@(C _ _)
  | n <= 1 || llen is <= n
  =   mapReduce n f op is
  ==. reduce op (f N) (map f (chunk n is))
  ==. reduce op (f N) (map f (C is N))
  ==. reduce op (f N) (f is `C` map f N)
  ==. reduce op (f N) (f is `C` N)
  ==. op (f is) (reduce op (f N) N)
  ==. op (f is) (f N)
  ==. f is
       ? rightId is
  *** QED

mRTheorem n f op rightId distrib is
  =   mapReduce n f op is
  ==. reduce op (f N) (map f (chunk n is))
  ==. reduce op (f N) (map f (C (take n is) (chunk n (drop n is))))
  ==. reduce op (f N) (C (f (take n is)) (map f (chunk n (drop n is))))
  ==. op (f (take n is)) (reduce op (f N) (map f (chunk n (drop n is))))
  ==. op (f (take n is)) (mapReduce n f op (drop n is))
  ==. op (f (take n is)) (f (drop n is))
      ? mRTheorem n f op rightId distrib (drop n is)
  ==. f ((take n is) ++ (drop n is))
      ? distrib (take n is) (drop n is)
  ==. f is
      ? appendTakeDrop n is
  *** QED
\end{code}

Append of Take and Drop
-----------------------

\begin{code}
{-@ automatic-instances appendTakeDrop @-}
{-@ appendTakeDrop :: i:Nat -> xs:{List a | i <= llen xs} ->
     {xs == (take i xs) ++ (drop i xs) }  @-}
appendTakeDrop i N
  = trivial
appendTakeDrop i (C x xs)
  | i == 0
  = trivial
appendTakeDrop i (C x xs)
  = appendTakeDrop (i-1) xs
\end{code}


List Definition
---------------

Built-in Lists are not supported for now.

(So does imports...)

\begin{code}
{-@ data List [llen] a = N | C {lhead :: a, ltail :: List a} @-}
data List a = N | C a (List a)

{-@ measure llen @-}
{-@ llen :: List a -> Nat @-}
llen N        = 0
llen (C _ xs) = 1 + llen xs
\end{code}


List Manipulation
------------------

\begin{code}
{-@ chunk :: i:Int -> xs:List a
    -> {v:List (List a) | if (i <= 1 || llen xs <= i) then (llen v == 1) else (llen v < llen xs) }
  / [llen xs] @-}
chunk i xs
  | i <= 1
  = C xs N
  | llen xs <= i
  = C xs N
  | otherwise
  = C (take i xs) (chunk i (drop i xs))

{-@ reflect drop @-}
{-@ drop :: i:Nat -> xs:{List a | i <= llen xs } -> {v:List a | llen v == llen xs - i } @-}
drop i N = N
drop i (C x xs)
  | i == 0
  = C x xs
  | otherwise
  = drop (i-1) xs

{-@ reflect take @-}
{-@ take :: i:Nat -> xs:{List a | i <= llen xs } -> {v:List a | llen v == i} @-}
take i N = N
take i (C x xs)
  | i == 0
  = N
  | otherwise
  = C x (take (i-1) xs)


{-@ reflect ++  @-}
N ++        ys = ys
(C x xs) ++ ys = x `C` (xs ++ ys)
\end{code}

