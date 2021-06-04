
<div class="hidden">
\begin{code}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--no-termination" @-}

module Sort () where

infixr 5 :< 

data OList a = Emp 
             | (:<) { oHd :: a, oTl :: OList a }

incList, decList, diffList :: [Int]
\end{code}

</div>

Case Study: Sorting Lists (Revisited)
-------------------------------------

<br>

**Recall our datatype for Ordered Lists** 

(Each) head-value `oHd` no bigger than _each_ tail-value in `oTl`

\begin{code}
{-@ data OList a = Emp
                 | (:<) {oHd :: a, oTl :: OList {v:a|oHd<=v}}
  @-}

okList :: OList Int
okList  = 1 :< 2 :< 3 :< Emp      -- legal 

badList :: OList Int
badList = 1 :< 3 :< 2 :< Emp      -- illegal
\end{code}

<div class="mybreak"><br></div>

Lets use it to implement <font color="#1569C7">**Insertion Sort**</font> and **Merge-Sort** 

Exercise: Insertion Sort actually Sorts
---------------------------------------

<br>

**Update our code to verify that the output is sorted**

\begin{code}
{- 
isort :: (Ord a) => [a] -> OList a
isort []       = Emp 
isort (x:xs)   = insert x (isort xs)

insert :: (Ord a) => a -> OList a -> OList a 
insert x (y:ys)
  | x <= y     = y : x : ys
  | otherwise  = y : insert x ys
insert x []    = [x] 
-}
\end{code}

**Exercise:** Yikes, so much red. Can you fix it?

Exercise: Merge Sort actually Sorts
-----------------------------------

**Exercise:** Update our code to verify that the output is sorted

\begin{code}
{- 
msort :: (Ord a) => [a] -> OList a
msort []   = []
msort [x]  = [x]
msort xs   = let (xs1, xs2) = split xs in
               merge (msort xs1) (msort xs2)

split :: [a] -> ([a], [a]) 
split (x:y:zs) = let (xs, ys) = split zs in 
                   (x:xs, y:ys) 
split xs       = (xs, [])

merge :: [a] -> [a] -> [a]  
merge xs []         = xs
merge [] ys         = ys
merge (x:xs) (y:ys)
  | x <= y          = x : merge xs (y:ys)
  | otherwise       = y : merge (x:xs) y
-}
\end{code}

Invariants In Constructors
--------------------------

<br>
<br>
<br>
<br>

Make illegal values unrepresentable!

<div class="mybreak"><br></div>

But tedious to have **multiple list types** ...

Abstracting Refinements
-----------------------

<br>
<br>
<br>
<br>

**Parameterize refinements over types**

i.e. _decouple_ the invariant from the data structure 

Abstracting Refinements
-----------------------

<div class="mybreak"><br></div>

**Parameterize refinements over types**

<div class="mybreak"><br></div>

\begin{spec}
                   data [a]<rel :: a -> a -> Bool> where
                     = []
                     | (:) { hd :: a
                           , tl :: [{v:a | rel hd v}]}
\end{spec}

Abstracting Refinements
-----------------------

<div class="mybreak"><br></div>

**Parameterize refinements over types**

\begin{spec}
                   data [a]<rel :: a -> a -> Bool> where
                     = []
                     | (:) { hd :: a
                           , tl :: [{v:a | rel hd v}]}
\end{spec}

**Refinement says `rel hd v` holds for _every_ value `v` in `tl`**

`rel` is `<=` means list is increasing 

`rel` is `>=` means list is increasing 

`rel` is `!=` means list is all-unique

Abstracting Refinements
-----------------------

<div class="mybreak"><br></div>

**Instantiate** refinements to get different invariants!

\begin{code}
{-@ type Incrs a = [a]<{\x y -> x <= y}> @-}
{-@ incList :: Incrs Int @-}
incList = [1,2,3]

{-@ type Decrs a = [a]<{\x y -> x >= y}> @-}
{-@ decList :: Decrs Int @-}
decList = [1,2,3]

{-@ type Diffs a = [a]<{\x y -> x != y}> @-}
{-@ diffList :: Diffs Int @-}
diffList = [3,1,1]
\end{code}

Abstract Refinements: Insertion Sort
------------------------------------

<div class="mybreak"><br></div>

**Inference FTW!**

\begin{code}
{-
{-@ isort' :: (Ord a) => [a] -> Incrs a @-}
isort' :: (Ord a) => [a] -> [a]
isort' []      = [] 
isort' (x:xs)  = insert' x (isort' xs)

insert' :: (Ord a) => a -> [a] -> [a]
insert' x (y:ys)
  | x <= y       = x : y : ys
  | otherwise    = y : insert' x ys
insert' x []     = [x]
-}
\end{code}

Abstract Refinements: Merge Sort
--------------------------------

**Inference FTW!**

\begin{code} 
{-  {-@ msort' :: (Ord a) => [a] -> Incrs a @-}
msort' :: (Ord a) => [a] -> [a]
msort' []   = []
msort' xs   = let (xs1, xs2) = split' xs in
                merge' (msort' xs1) (msort' xs2)

split' :: [a] -> ([a], [a]) 
split' (x:y:zs) = let (xs, ys) = split' zs in 
                   (x:xs, y:ys) 
split' xs       = (xs, [])

merge' :: (Ord a) => [a] -> [a] -> [a]  
merge' xs []         = xs
merge' [] ys         = ys
merge' (x:xs) (y:ys)
  | x <= y           = x : merge' xs (y:ys)
  | otherwise        = y : merge' (x:xs) ys    -}
\end{code}

Recap: Invariant via Refined Constructors
-----------------------------------------

<br>
<br>
<br>

\begin{spec}<div/>
             Emp  :: OList a
             (:<) :: oHd:a -> OList {v:a |oHd < v} -> OList a 
\end{spec}

<div class="mybreak"><br></div>

**Invariant _Checked_ at Construction and _Instantiated_ at Pattern-Matching**

Simply by *applying* and *un-applying* refined constructor type!

Plan
----

<br> 

**Part I:** [Refinements 101](02-refinements.html)

Case Study: [Vector Bounds](03-example-vectors.html)

**Part II:** [Properties of Structures](04-data-properties.html)

Case Study: [Sorting](05-example-sort.html), [Interpreter](06-example-interpreter.html)

**Part III:** [Invariants of Data Structures](07-data-legal.html)

Case Study: [Sorting actually Sorts](08-example-sort.html)

**Part IV:** **[Termination](09-termination.html)** and [Correctness Proofs](10-reflection.html)

Case Study: [Optimizing Arithmetic Expressions](11-example-opt.html)
