<div class="hidden">
\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}

module Examples
       ( average
       , length
       , hwAverage
       )
       where

import Prelude hiding (map, length)

reduce :: (a -> b -> a) -> a -> [b] -> a
reduce f acc []     = acc 
reduce f acc (x:xs) = let acc' = f acc x 
                      in
                          reduce f acc' xs
\end{code}

</div>

Properties of Data Structures
-----------------------------

<br>
<br>

**So far: Refinements for properties of basic values like `Int`**

e.g. `{v:Int | lo <= v && v < hi}`

Properties of Data Structures
-----------------------------

<br>
<br>

**So far: Refinements for properties of basic values like `Int`**

e.g. `{v:Int | lo <= v && v < hi}`

<br>

**Next: How to describe properties of data structures**

Example: List `average`
-----------------------

<br>
<br>

**Suppose we have a built-in `div` operator:**

`div :: Int -> {d:Int| d /= 0} -> Int`

<div class="mybreak"><br></div>

**Input refinement specifies Pre-condition** 

Denominator `d` is non-zero


Example: List `average`
-----------------------

<div class="mybreak"><br></div>

Lets use `div` to write an `average` function

\begin{code}
{-@ average :: [Int] -> Int @-}
average xs  = let total = reduce (+) 0 xs    -- sum xs
                  n     = length xs
              in  
                  div total n

length       :: [a] -> Int
length []    = 0
length (_:t) = 1 + length t 
\end{code}

**Exercise:** Why is there an error? 

Properties of Structures
------------------------

<div class="mybreak"><br></div>

How to describe the *size* of a list?

Properties of Structures
------------------------

<div class="mybreak"><br></div>

How to describe the *size* of a list?

<div class="mybreak"><br></div>

**Allow (some) functions inside refinements**

Properties of Structures
------------------------

<div class="mybreak"><br></div>

**Allow (some) functions inside refinements**

\begin{code}
{-@ measure length @-}
\end{code}

Which lets us define a type for **Non-Empty Lists**

\begin{code}
{-@ type ListNE a = {v:[a] | 0 < length v} @-}
\end{code}

<div class="mybreak"><br></div>

**Exercise:** Go back and *fix* `average` so it checks?

Properties of Structures
------------------------

<br>

**Measures Generate _Refined_ Constructors**

\begin{spec}<div/>
       []  :: {v:[a] | length v = 0}
       (:) :: a -> t:[a] -> {v:[a] | length v = 1 + length t}
\end{spec}

Properties of Structures
------------------------

<br>

**Measures Generate _Refined_ Constructors**

\begin{spec}<div/>
       []  :: {v:[a] | length v = 0}
       (:) :: a -> t:[a] -> {v:[a] | length v = 1 + length t}
\end{spec}

<div class="mybreak"><br></div>

**Where `length` is uninterpreted in refinement Logic**

Properties of Structures
------------------------

<br>

**Measures Generate _Refined_ Constructors**

\begin{spec}<div/>
       []  :: {v:[a] | length v = 0}
       (:) :: a -> t:[a] -> {v:[a] | length v = 1 + length t}
\end{spec}

<div class="mybreak"><br></div>

**Where `length` is uninterpreted in refinement Logic**

<div class="mybreak"><br></div>

Now plain refinement typing "just works" for properties of structures!


Example: `map` over Lists
-------------------------

A datatype for homework scores, function to compute their average:

\begin{code}
data Hw = Hw { hwName  :: String  -- ^ Student's Name
             , hwScore :: Int     -- ^ Student's Score  
             }

hwAverage :: [Hw] -> Int
hwAverage hws = average (map hwScore hws)

map :: (a -> b) -> [a] -> [b] 
map f []     = []
map f (x:xs) = f x : map f xs
\end{code}

**Exercise:** What's the problem here? Can you fix it?

Recap: Properties of Structures
-------------------------------

<div class="mybreak"><br></div>

**Measures specify properties as functions over Structures**

\begin{spec}<div/>
                 {-@ measure length @-}
                 length       :: [a] -> Nat
                 length []    = 0
                 length (_:t) = 1 + length t
\end{spec}

Recap: Properties of Structures
-------------------------------

<div class="mybreak"><br></div>

**Measures specify properties as functions over Structures**

\begin{spec}<div/>
                 {-@ measure length @-}
                 length       :: [a] -> Nat
                 length []    = 0
                 length (_:t) = 1 + length t
\end{spec}

<div class="mybreak"><br></div>

**Refined Constructor Types**

\begin{spec}<div/>
       []  :: {v:[a] | length v = 0}
       (:) :: a -> t:[a] -> {v:[a] | length v = 1 + length t}
\end{spec}

Recap: Properties of Structures
-------------------------------

<div class="mybreak"><br></div>

**Refined Constructor Types**

\begin{spec}<div/>
       []  :: {v:[a] | length v = 0}
       (:) :: a -> t:[a] -> {v:[a] | length v = 1 + length t}
\end{spec}

**Generalize Properties during Construction** 

When *applying* type of `[]` and `(:)` 

<div class="mybreak"><br></div>

Recap: Properties of Structures
-------------------------------

<div class="mybreak"><br></div>

**Refined Constructor Types**

\begin{spec}<div/>
       []  :: {v:[a] | length v = 0}
       (:) :: a -> t:[a] -> {v:[a] | length v = 1 + length t}
\end{spec}

**Generalize Properties during Construction** 

When *applying* type of `[]` and `(:)` 

<div class="mybreak"><br></div>

**Instantiate Properties during Pattern-Matching** 

When *unapplying* type of  `[]` and `(:)` 

Multiple Measures are Conjoined
-------------------------------

<br>
<br>

**We can write *multiple* measures for lists**

\begin{spec}<div/>
                      length :: [a] -> Int 
                      elems  :: [a] -> Set a
\end{spec}

Multiple Measures are Conjoined
-------------------------------

**We can write *multiple* measures for lists**

\begin{spec}<div/>
                      length :: [a] -> Int 
                      elems  :: [a] -> Set a
\end{spec}

**Constructor refinements are _conjoined_**

\begin{spec}<div/>
  []  :: {v:[a] | length v = 0 && elems v = empty}
  (:) :: x:a -> xs:[a] -> {v:[a] |  length v = 1 + length xs
                                 && elems v  = addElem x  xs }
\end{spec}

**Allows programmer to easily _extend_ properties** [continue ...](00-plan.html)
