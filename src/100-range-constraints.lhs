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
  | lo < hi   = let tail = range (lo + 1) hi  ----------- (1)
                in  lo : tail -------------------- (2l,2t,2r)
  | otherwise = [] -------------------------------------- (3)  
\end{spec}

**Polymorphic Type and Instance for "Cons"**

\begin{spec}<div/>
    (:) :: a -> [a] -> [a]
    (:) :: {v:Int|K(v)} -> [{v:Int|K(v)}] -> [{v:Int|K(v)}]
\end{spec}

where `a` replaced with unknown `{v:Int|K(v)}`


Subtyping Constraints
---------------------

\begin{spec}<div/>
range :: lo:_ -> hi:{lo <= hi} -> [{v:_ | lo <= v && v < hi}] 
range lo hi
  | lo < hi   = let tail = range (lo + 1) hi  ----------- (1)
                in  lo : tail -------------------- (2l,2t,2r)
  | otherwise = [] -------------------------------------- (3)  
\end{spec}

Yields the **Subtyping Constraints**

\begin{spec}<div/>
lo < hi |- {v:Int|v = lo+1}     <: {v:Int|v <= hi}    -- (1)
lo < hi |- {v:Int|v = lo}       <: {v:Int|K(v)}       -- (2l)
lo < hi |- [{v:Int|lo+1<=v<hi}] <: [{v:Int|K(v)}]     -- (2t)
lo < hi |- [{v:Int|K(v)}]       <: [{v:Int|lo<=v<hi}] -- (2r)
lo >=hi |- [{v:Int|false}]      <: [{v:Int|lo<=v<hi}] -- (3)
\end{spec}

Horn Constraints
----------------

\begin{spec}<div/>
range :: lo:_ -> hi:{lo <= hi} -> [{v:_ | lo <= v && v < hi}] 
range lo hi
  | lo < hi   = let tail = range (lo + 1) hi  ----------- (1)
                in  lo : tail -------------------- (2l,2t,2r)
  | otherwise = [] -------------------------------------- (3)  
\end{spec}

**List Subtyping reduces to element-wise Subtyping**

\begin{spec}<div/>
lo < hi => (v = lo+1)   => (v <= hi)                  -- (1)
lo < hi => (v = lo)     => K(v)                       -- (2l)
lo < hi => (lo+1<=v<hi) => K(v)                       -- (2t)
lo < hi => K(v)         => (lo <= v < hi)             -- (2r)
lo >=hi => false        => (lo <= v < hi)             -- (3)
\end{spec}

**Solution:** `K(v) := lo <= v < hi`

