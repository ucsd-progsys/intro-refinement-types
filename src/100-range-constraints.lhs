<div class="hidden">
\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}

module Bonus where

incr :: Int -> Int 
incr x = x + 1
\end{code}
</div>

Bonus Slides: Verifying `range`
-------------------------------

\begin{spec}<div/>
range :: lo:_ -> hi:{lo <= hi} -> [{v:_ | lo <= v && v < hi}] 
range lo hi
  | lo < hi   = let tail = range (lo + 1) hi  ---------- (1)
                in  lo : tail -------------------------- (2-lo, 2-tail, 2-res)
  | otherwise = [] ------------------------------------- (3)  
\end{spec}

**Polymorphic Type for "Cons"**

\begin{spec}<div/>
    (:) :: a -> [a] -> [a]
\end{spec}

**Polymorphic Instance for "Cons"** 

\begin{spec}<div/>
    (:) :: {v:Int|K(v)} -> [{v:Int|K(v)}] -> [{v:Int|K(v)}]
\end{spec}

where `a` replaced with unknown `{v:Int|K(v)}`


Subtyping Constraints
---------------------

\begin{spec}<div/>
range :: lo:_ -> hi:{lo <= hi} -> [{v:_ | lo <= v && v < hi}] 
range lo hi
  | lo < hi   = let tail = range (lo + 1) hi  ---------- (1)
                in  lo : tail -------------------------- (2-lo, 2-tail, 2-res)
  | otherwise = [] ------------------------------------- (3)  
\end{spec}

Yields the **Subtyping Constraints**

\begin{spec}<div/>
lo < hi |- {v:Int|v = lo+1}     <: {v:Int|v <= hi}    -- (1)
lo < hi |- {v:Int|v = lo}       <: {v:Int|K(v)}       -- (2-lo)
lo < hi |- [{v:Int|lo+1<=v<hi}] <: [{v:Int|K(v)}]     -- (2-tail)
lo < hi |- [{v:Int|K(v)}]       <: [{v:Int|lo<=v<hi}] -- (2-res)
lo >=hi |- [{v:Int|false}]      <: [{v:Int|lo<=v<hi}] -- (3)
\end{spec}

Horn Constraints
----------------

\begin{spec}<div/>
range :: lo:_ -> hi:{lo <= hi} -> [{v:_ | lo <= v && v < hi}] 
range lo hi
  | lo < hi   = let tail = range (lo + 1) hi  ---------- (1)
                in  lo : tail -------------------------- (2-lo, 2-tail, 2-res)
  | otherwise = [] ------------------------------------- (3)  
\end{spec}

**List Subtyping reduces to element-wise Subtyping**

\begin{spec}<div/>
lo < hi => (v = lo+1)   => (v <= hi)                  -- (1)
lo < hi => (v = lo)     => K(v)                       -- (2-lo)
lo < hi => (lo+1<=v<hi) => K(v)                       -- (2-tail)
lo < hi => K(v)         => (lo <= v < hi)             -- (3)
lo >=hi => false        => (lo <= v < hi)             -- (3)
\end{spec}

**Solution**

\begin{spec}<div/>
K(v) := lo <= v < hi
\end{spec}

