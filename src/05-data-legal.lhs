
<div class="hidden">
\begin{code}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--no-warnings"    @-}
{-@ LIQUID "--no-termination" @-}

module Sort where

import Prelude hiding (sum, length, map, filter, foldr, foldr1)
import qualified Data.Set as S -- hiding (elems, insert)
infixr 5 :< 
-- insert, insertE :: (Ord a) => a -> [a] -> [a]
-- sort, sortE     :: (Ord a) => [a] -> [a]

{-@ measure length @-}
{-@ length :: [a] -> Nat @-}
length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs
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

**Next: Invariants of structures**

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

<div class="mybreak"><br></div>

**Generalize Invariant during Construction**

i.e. *applying* type of `[]` and `(:)` 

Strategy: Encode Invariants in *Constructors*
---------------------------------------------

<br>

**Reuse the idea for properties!**

Verification works out _exactly_ as before 

<div class="mybreak"><br></div>

**Generalize Invariant during Construction**

i.e. *applying* type of `[]` and `(:)` 

<div class="mybreak"><br></div>

**Instantiate Invariant during Pattern-Matching** 

i.e. *unapplying* type of `[]` and `(:)` 


Lets write a type for Ordered Pairs
-----------------------------------

\begin{code}
data OrdPair = MkOP {opX :: Int, opY :: Int}

-- | Constructing Pairs
okPair  = MkOP 2 4  -- legal
badPair = MkOP 4 2  -- illegal

-- | Destructing Pairs
{-@ checkPair :: OrdPair -> {v:Int | 0 < v} @-}
checkPair (MkOP a b) = b - a

-- | Refine `OrdPair` to only allow legal pairs where opX < opY
{-@ data OrdPair = MkOP { opX :: Int, opY :: Int} @-}
\end{code}

**Exercise:** Refine `OrdPair` to only allow *legal* pairs where `opX < opY`

Invariant via Refined Constructors
----------------------------------

<br>
<br>
<br>

**Reuse Strategy: Refinements Strengthen Constructors!** 

\begin{spec}<div/>
          MkOP :: opX:Int -> opY:{v:Int|opX < v} -> OrdPair  
\end{spec}


Invariant via Refined Constructors
----------------------------------

<br>
<br>

**Reuse Strategy: Refinements Strengthen Constructors!** 

\begin{spec}<div/>
          MkOP :: opX:Int -> opY:{v:Int|opX < v} -> OrdPair  
\end{spec}

<div class="mybreak"><br></div>

**Invariant _Checked_ At Construction**

By *applying* constructor *pre-condition* 

Invariant via Refined Constructors
----------------------------------

<div class="mybreak"><br></div>

**Reuse Strategy: Refinements Strengthen Constructors!** 

\begin{spec}<div/>
          MkOP :: opX:Int -> opY:{v:Int|opX < v} -> OrdPair  
\end{spec}

<div class="mybreak"><br></div>

**Invariant _Checked_ At Construction**

By *applying* constructor *pre-condition* 

<div class="mybreak"><br></div>

**Invariant _Instantiated_ At Pattern-Matching**

By *unapplying* constructor *pre-condition*

Refined Data: CSV Tables
------------------------

**A datatype to represent Comma-Separated-Value Tables** 

<div class="mybreak"><br></div>

\begin{code}
data Csv = MkCsv { hdrs :: [String]
                 , rows :: [[Int]]
                 }

scores   = MkCsv 
  { hdrs =  ["Id", "Midterm", "Final"] 
  , rows = [[  1 ,       25 ,      88]
           ,[  2 ,       27 ,      83]
           ,[  3 ,       19 ,      93]]
  }
\end{code}

Exercise: Legal CSV Tables
--------------------------

**A Legal CSV table has rows without _missing_ or _extra_ values** 

\begin{code}
badScores = MkCsv 
  { hdrs  =  ["Id", "Midterm", "Final"] 
  , rows  = [[  1 ,                 88]
            ,[  2 ,       27 ,      83]
            ,[  3 ,       19          ]]
  }

{-@ data Csv = MkCsv { hdrs :: [String]
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
  
    MkCsv :: hdrs:[String] -> [{row:[Int]|len row = len hdrs}] -> Csv
  
    Emp  :: OList a
    (:<) :: oHd:a -> OList {v:a |oHd < v} -> OList a 
\end{spec}


Recap: Invariant via Refined Constructors
----------------------------------

<div class="mybreak"><br></div>

\begin{spec}<div/>
    OP    :: opX:Int -> opY:{v:Int |opX < v} -> OrdPair  
   
    MkCsv :: hdrs:[String] -> [{row:[Int]|len row = len hdrs}] -> Csv
  
    Emp   :: OList a
    (:<)  :: oHd:a -> OList {v:a |oHd < v} -> OList a 
\end{spec}

<div class="mybreak"><br></div>

**Invariant _Checked_ at Construction and _Instantiated_ at Pattern-Matching**

Simply by *applying* and *un-applying* refined constructor type! [Continue...](00-plan.html)
