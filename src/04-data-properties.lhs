<div class="hidden">
\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}

module Examples
       ( average
       , length
       )
       where
\end{code}
</div>

Properties of Data Structures
-----------------------------

<div class="mybreak"><br></div>

**So far:** Refinements just describe properties of basic values like `Int`

e.g. `{v:Int | lo <= v && v < hi}`

<div class="mybreak"><br></div>

**Next:** How to describe properties of complex data structures

Example: List `average`
-----------------------

<div class="mybreak"><br></div>

Suppose we have a built-in `div` operator:

\begin{spec}<div/>
      div :: Int -> {d:Int| d /= 0} -> Int
\end{spec}

<div class="mybreak"><br></div>

**Input refinement specifies Pre-condition** 

Denominator `d` is non-zero


Example: List `average`
-----------------------

<div class="mybreak"><br></div>

Lets use `div` to write an `average` function

\begin{code}
{-@ average :: [Int] -> Int @-}
average xs  = let total = reduce (+) 0 xs
                  n     = length xs
              in  
                  div total n

length       :: [a] -> Int
length []    = 0
length (_:t) = 1 + length t 
\end{code}

**Exercise** Why is there an error? 

Properties of Structures
------------------------

<div class="mybreak"><br></div>

How to describe the *size* of a list?

Properties of Structures
------------------------

<div class="mybreak"><br></div>

How to describe the *size* of a list?

<div class="mybreak"><br></div>

**Allow (some) Functions Inside Refinements**

Properties of Structures
------------------------

<div class="mybreak"><br></div>

**Allow (some) Functions Inside Refinements**

\begin{code}
{-@ measure length @-}
\end{code}

Which lets us define a type alias

\begin{code}
{-@ type ListNE a = {v:[a] | 0 < length v} @-}
\end{code}

<div class="mybreak"><br></div>

**Exercise:** Can you go back and *fix* `average` so it checks?

Measures Yield Refined Constructors
-----------------------------------

<div class="mybreak"><br></div>

**Measures Allow (some) Functions Inside Refinements**

\begin{spec}<div/>
[]  :: {v:[a] | length v = 0}
(:) :: a -> t:[a] -> {v:[a] | length v = 1 + length t}
\end{spec}

Where `length` is **uninterpreted** in refinement Logic

<div class="mybreak"><br></div>

Now plain refinement typing "just works" for properties of strucures!


Example: `map` over Lists
-------------------------

Here's a datatype that defines a list of homework scores

\begin{code}
data Hw = Hw { getName  :: String  -- ^ Student's Name
             , getScore :: Int     -- ^ Student's Score 
             }
\end{code}

Here's a function to compute the average of a collection of scores

\begin{code}
hwAverage :: [Hw] -> Int
hwAverage hws = average (map getScore hws)

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
   length       :: [a] -> Int
   length []    = 0
   length (_:t) = 1 + length t
\end{spec}

Recap: Properties of Structures
-------------------------------

<div class="mybreak"><br></div>

**Measures specify properties as functions over Structures**

\begin{spec}<div/>
   {-@ measure length @-}
   length       :: [a] -> Int
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

**Generalize Properties** 

During *construction* i.e. *applying* `[]` and `(:)` 

<div class="mybreak"><br></div>

Recap: Properties of Structures
-------------------------------

<div class="mybreak"><br></div>

**Refined Constructor Types**

\begin{spec}<div/>
   []  :: {v:[a] | length v = 0}
   (:) :: a -> t:[a] -> {v:[a] | length v = 1 + length t}
\end{spec}

**Generalize Properties** 

During *construction* i.e. *applying* `[]` and `(:)` 

<div class="mybreak"><br></div>

**Instantiate Properties** 

During *destruction* i.e. *pattern-matching* `[]` and `(:)` 

Plan
----

<br>
<br>

**Part I:** [Refinements 101](02-refinements.html)

Case Study: [Vector Bounds](03-example-vectors.html)

<br>

**Part II:** [Properties of Structures](04-data-properties.html)

Case Study: **[Sorting](05-example-sort.html)**, [Interpreter](06-example-interpreter.html)


