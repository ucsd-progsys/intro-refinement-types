
<div class="hidden">
\begin{code}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--no-termination" @-}

module Sort where

import Prelude hiding (sum, length, map, filter, foldr, foldr1)
import qualified Data.Set as S -- hiding (elems, insert)

insert, insertE :: (Ord a) => a -> [a] -> [a]
sort, sortE     :: (Ord a) => [a] -> [a]

\end{code}

</div>

Invariants of Data Structures
-----------------------------

<div class="mybreak"><br></div>

**So far**

Properties of basic values

e.g. `{v:Int | lo <= v && v < hi}`

Properties of structures 

e.g. `{v:[Int] | length v == 1 + length xs}`

Invariants of Data Structures
-----------------------------

<div class="mybreak"><br></div>

**So far**

Properties of basic values

e.g. `{v:Int | lo <= v && v < hi}`

Properties of structures 

e.g. `{v:[Int] | length v == 1 + length xs}`

<div class="mybreak"><br></div>

**Next**

Invariants of structures

e.g. a `Date` is *valid* or a `List` is *sorted*


Recall: Properties of Structures
--------------------------------

<br>
<br>
<br>

**Refined Constructor Types**

\begin{spec}<div/>
            []  :: {v:[a] | length v = 0}
            (:) :: a -> t:[a] -> {v:[a] | length v = 1 + length t}
\end{spec}


Recall: Properties of Structures
--------------------------------

<div class="mybreak"><br></div>

**Refined Constructor Types**

\begin{spec}<div/>
            []  :: {v:[a] | length v = 0}
            (:) :: a -> t:[a] -> {v:[a] | length v = 1 + length t}
\end{spec}

<div class="mybreak"><br></div>

**Generalize Properties during _Construction_** 

i.e. *applying*  type of `[]` and `(:)` 

<div class="mybreak"><br></div>

**Instantiate Properties during _Pattern-Matching_** 

i.e. *unapplying*  type of  `[]` and `(:)` 

Strategy: Encode Invariants in *Constructors*
---------------------------------------------

<br>
<br>
<br>

**Reuse the idea for properties!**

Verification works out _exactly_ as before 


Strategy: Encode Invariants in *Constructors*
---------------------------------------------

<br>

**Reuse the idea for properties!**

Verification works out _exactly_ as before 

<br>

**Generalize Invariant during Construction**

i.e. *applying* type of `[]` and `(:)` 

<br>

**Instantiate Invariant during Pattern-Matching** 

i.e. *unapplying* type of `[]` and `(:)` 

Invariant: Ordered Pairs
------------------------

Lets write a type for **ordered pairs**

\begin{code}
data OrdPair = OP {opX :: Int, opY :: Int}

-- | Constructing Pairs
okPair  = OP 2 4  -- legal
badPair = OP 4 2  -- illegal

-- | Destructing Pairs
checkPair (OP a b) 
  | a < b     = True 
  | otherwise = impossible "illegal OrdPair!"

-- | Refine `OrdPair` to only allow legal pairs where opX < opY
{-@ data OrdPair = OP { opX :: Int, opY :: Int} @-}
\end{code}

**Exercise:** Refine `OrdPair` to only allow *legal* pairs where `opX < opY`

Invariant via Refined Constructors
----------------------------------

<br>
<br>
<br>

**Reuse Strategy: Refinements Strengthen Constructors!** 

\begin{spec}<div/>
              OP :: opX:Int -> opY:{v:Int|opX < v} -> OrdPair  
\end{spec}


Invariant via Refined Constructors
----------------------------------

<br>
<br>

**Reuse Strategy: Refinements Strengthen Constructors!** 

\begin{spec}<div/>
              OP :: opX:Int -> opY:{v:Int|opX < v} -> OrdPair  
\end{spec}

<div class="mybreak"><br></div>

**Invariant _Checked_ At Construction**

By *applying* constructor pre-condition 

Invariant via Refined Constructors
----------------------------------

<div class="mybreak"><br></div>

**Reuse Strategy: Refinements Strengthen Constructors!** 

\begin{spec}<div/>
              OP :: opX:Int -> opY:{v:Int|opX < v} -> OrdPair  
\end{spec}

<div class="mybreak"><br></div>

**Invariant _Checked_ At Construction**

By *applying* constructor pre-condition 

<div class="mybreak"><br></div>

**Invariant _Instantiated_ At Pattern-Matching**

By *unapplying* constructor pre-condition

Refined Data: CSV Tables
------------------------

<br>

**A datatype to represent Comma-Separated-Value Tables** 

<div class="mybreak"><br></div>

\begin{code}
data Csv = Csv { hdrs :: [String]
               , rows :: [[Int]]
               }

scores   = Csv 
  { hdrs =  ["Id", "Midterm", "Final"] 
  , rows = [[  1 ,       25 ,      88]
           ,[  2 ,       27 ,      83]
           ,[  3 ,       19 ,      93]]
  }
\end{code}

Exercise: Legal CSV Tables
--------------------------

<br>

**A Legal CSV table has rows without _missing_ values** 

<div class="mybreak"><br></div>

\begin{code}
badScores = Csv 
  { hdrs  =  ["Id", "Midterm", "Final"] 
  , rows  = [[  1 ,                 88]
            ,[  2 ,       27 ,      83]
            ,[  3 ,       19          ]]
  }

{-@ data Csv = Csv { hdrs :: [String]
                   , rows :: [[Int]]
                   }   
  @-}
\end{code}

<div class="mybreak"><br></div>

**Exercise:** Refine `Csv` so `badScores` is rejected.

Ordered Lists
-------------

<br>
<br>
<br>
<br>

**Define a list type whose legal values are ordered**

Ordered Lists
-------------

<div class="mybreak"><br></div>

**Define a list type whose legal values are ordered**

\begin{code}
data OList a = Emp 
             | (:<) { oHd :: a, oTl :: OList a }

okList :: OList Int
okList  = 1 :< 2 :< 3 :< Emp      -- legal 

badList :: OList Int
badList = 1 :< 3 :< 2 :< Emp      -- illegal

{-@ data OList a = Emp
                 | (:<) {oHd :: a, oTl :: OList a}
  @-}
\end{code}

**Exercise:** Refine `OList` so (only) `badList` is rejected.

Recap: Invariant via Refined Constructors
----------------------------------

<br>
<br>
<br>

\begin{spec}<div/>
      OP   :: opX:Int -> opY:{v:Int |opX < v} -> OrdPair  
  
      Csv  :: hdrs:[String] -> [{row:[Int]|len row = len hdrs}] -> Csv
  
      Emp  :: OList a
      (:<) :: oHd:a -> OList {v:a |oHd < v} -> OList a 
\end{spec}


Recap: Invariant via Refined Constructors
----------------------------------

<div class="mybreak"><br></div>

\begin{spec}<div/>
      OP   :: opX:Int -> opY:{v:Int |opX < v} -> OrdPair  
  
      Csv  :: hdrs:[String] -> [{row:[Int]|len row = len hdrs}] -> Csv
  
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

Case Study: **[Sorting revisited](08-example-sort.html)**

**Part IV:** [Termination](10-termination.html) and [Correctness Proofs](11-reflection.html)

Case Study: [Optimizing Arithmetic Expressions](12-example-opt.html)