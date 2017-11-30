Data Types
==========

\begin{code}
{-# LANGUAGE TupleSections    #-}
{-@ LIQUID "--no-termination" @-}
module DataTypes where

import Prelude hiding (length, sum, take)
\end{code}

<div class="hidden">

\begin{code}

head       :: List a -> a
tail       :: List a -> List a
headt      :: List a -> a
tailt      :: List a -> List a
impossible :: String -> a
avg        :: List Int -> Int
take       :: Int -> List a -> List a 


sum :: List Int -> Int 
sum Emp        = 0 
sum (x ::: xs) = x + sum xs

{-@ data List [length] @-}
{-@ invariant {v: List a | 0 <= length v} @-}

{-@ type Nat      = {v:Int | v >= 0} @-}
{-@ type Pos      = {v:Int | v >  0} @-}

{-@ impossible :: {v:_ | false} -> a @-}
impossible = error

{-@ safeDiv :: Int -> {v:Int | v /= 0} -> Int   @-}
safeDiv :: Int -> Int -> Int 
safeDiv _ 0 = impossible "divide-by-zero"
safeDiv x n = x `div` n

\end{code}

</div>

Example: Lists
==========

<br>

<div class="fragment">
Lets define our own `List` data type:

<br>
\begin{code}
data List a  = Emp
             | (:::) a (List a)
             deriving (Eq, Ord, Show)

infixr 9 :::
\end{code}
</div>



Specifying the Length of a List
==========

<div class="fragment">
**Measure**

Haskell function with *a single equation per constructor*
</div>

<br>

\begin{code}
{-@ measure length          @-}
{-@ length :: List a -> Nat @-}
length     :: List a -> Int
length Emp      = 0
length (_:::xs) = 1 + length xs
\end{code}


Specifying the Length of a List
==========


**Measure:** *Strengthens* type of data constructor

<div class="fragment">

\begin{spec} <div/>
data List a where

  Emp   :: {v:List a | length v = 0}

  (:::) :: x:a -> xs:List a
        -> {v:List a | length v = 1 + length xs}
\end{spec}

</div>

Example: *Partial* Functions
==========

Fear `head` and `tail` no more!

<div class="fragment">
\begin{code}
{-@ head     :: List a -> a @-}
head (x:::_) = x
head _       = impossible "head"

{-@ tail      :: List a -> List a @-}
tail (_:::xs) = xs
tail _        = impossible "tail"
\end{code}

<br>
**Q:** Write types for `head` and `tail` that verify `impossible`.
</div>

Naming Non-Empty Lists
==========


<br>

A convenient *type alias*

<br>

\begin{code}
{-@ type ListNE a = {v:List a| 0 < length v} @-}
\end{code}

Totality Checking in Liquid Haskell 
==========

<div class="fragment">
\begin{code}

{-@ headt      :: List a -> a @-}
headt (x:::_)  = x

{-@ tailt      :: ListNE a -> List a @-}
tailt (_:::xs) = xs
\end{code}

<br>

Partial Functions are _automatically_ detected!

</div>



Back to `average`
==========


<br>

\begin{code}
{-@ avg    :: List Int -> Int @-}
avg xs     = safeDiv total n
  where
    total  = sum    xs
    n      = length xs         -- returns a Nat
\end{code}

<br> 
**Q:** Write type for `avg` that verifies safe division.

In bounds `take`
==========


**Q:** Use measures to specify a good type for `take`
<br>

\begin{code}
{-@ take :: i:Int -> xs:List a -> List a @-} 
take 0 Emp      = Emp 
take i (x:::xs) = if i == 0 then Emp else x:::(take (i-1) xs)
take i Emp      = impossible "Out of bounds indexing!" 
\end{code}



Catch the The Heartbleed Bug!
==========


Assuming the library `Text` types...

<br>
<div class="hidden">
\begin{code}
data Text 
pack :: String -> Text 
takeWord16 :: Int -> Text -> Text 
pack = undefined 
takeWord16 = undefined 
\end{code}
</div>
\begin{code}
{-@ measure tlen      :: Text -> Int                               @-}

{-@ assume pack       :: i:String -> {o:Text | len i == tlen o }   @-}
{-@ assume takeWord16 :: i:Nat -> {v:Text | i <= tlen v} -> Text @-}
\end{code}


<br>
... HeartBleed **cannot** happen!
<br>

\begin{code}
safeTake   = takeWord16 2  (pack "hat")
unsafeTake = takeWord16 10 (pack "hat")
\end{code}

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Recap
==========


<br>
<br>

- **1. Refinements:** Types + Predicates
- **2. Automation:** SMT Implication
- <div class="fragment">**3. Measures:** Specify Properties of Data</div>
- [4. Case Study: Insert Sort](04-insert-sort.html) 

