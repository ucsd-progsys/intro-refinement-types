
<div class="hidden">
\begin{code}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--no-termination" @-}

module Sort where

msort :: Ord a => [a] -> OList a 
split :: [a] -> ([a], [a])
merge :: Ord a => OList a -> OList a -> OList a

data OList a = Emp 
             | (:<) { oHd :: a, oTl :: OList a }
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
isort :: (Ord a) => [a] -> [a]
isort []       = []
isort (x:xs)   = insert x (isort xs)

insert :: (Ord a) => a -> OList a -> OList a 
insert x (y:ys)
  | x <= y     = y : x : ys
  | otherwise  = y : insert x ys
insert x []    = [x] 
\end{code}

**Exercise:** Yikes, so much red. Can you fix it?

Exercise: Merge Sort actually Sorts
-----------------------------------

**Exercise:** Update our code to verify that the output is sorted

\begin{code}
msort :: [a] -> OList a
msort []   = []
msort [x]  = [x]
msort xs   = let (xs1, xs2) = split xs
             in 
                 merge (msort xs1) (msort xs2)

split :: [a] -> ([a], [a]) 
split (x:y:zs) = let (xs, ys) = split zs 
                 in 
                     (x:xs, y:ys) 
split xs       = (xs, [])

merge :: [a] -> [a] -> [a]  
merge xs []         = xs
merge [] ys         = ys
merge (x:xs) (y:ys)
  | x <= y          = x : merge xs (y:ys)
  | otherwise       = y : merge (x:xs) y
\end{code}