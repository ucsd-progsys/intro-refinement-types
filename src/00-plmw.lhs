
<div class="hidden">
\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}


module IntroToRefinementTypes
       ( sum
       , sumBad
       , sumInfer
       , sumHO
       , average
       , insertSort
       )
       where

import Prelude hiding (foldr, map, sum, length, (!))
import qualified Data.Vector as V
\end{code}
</div>

<div class="slide">

<br>
<br>
<br>
<br>
<br>


<p align=center>
<h1 style="border-bottom:none">An Introduction to Refinement Types</h1>

<br>

<h4 style="border-bottom:none"><i>Ranjit Jhala (UCSD)</i></h4>
</p>

</div>


Whats this?
-----------

<br>


<p align=center>
<img src="img/firstbug-crop.jpg" height=300px/>
</p>

The First *Bug*
---------------

<br>

<p align=center>
<img src="img/firstbug-crop2.jpg" height=200px/>
</p>

<p align=center>
**Page from Harvard Mark II log**

A *dead moth* removed from the device
</p>


Fast forward to Present Day
---------------------------

<br>

<p align=center>
<img src="img/news-bug-1.png" height=250px>
</p>

Fast forward to Present Day
---------------------------

<br>

<p align=center>
<img src="img/news-bug-2.png" height=275px>
</p>

Fast forward to Present Day
---------------------------

<br>

<p align=center>
<img src="img/news-bug-3.png" height=300px>
</p>


Programming Languages Research
------------------------------

<p align=center>
  <img src="img/george-orwell.jpg" height=240px>

  **George Orwell (1984)**
</p>

<img src="img/thoughtcrime.png" height=60px>



Modern Languages
----------------

<br>

F#

Rust

Scala

OCaml

Haskell


Modern Languages
----------------

<br>

Static Typing

<br>

First-class Functions

<br>

Immutability by Default

<br>

Make **good** designs **easy** and **bad** designs **hard**

Modern Languages?
-----------------

<br>

**Alas ... well-typed programs can go very wrong!**

Well-typed programs can go very wrong!
-----------------

<br>

Well-typed programs can go very wrong!
-----------------

<br>

Divide-by-zero

Well-typed programs can go very wrong!
-----------------

<br>

Divide-by-zero

Keys missing in Maps

Well-typed programs can go very wrong!
-----------------

<br>

Divide-by-zero

Keys missing in Maps

Pattern-match failures

Well-typed programs can go very wrong!
-----------------

<br>

Divide-by-zero

Keys missing in Maps

Pattern-match failures

Buffer overflows (!)



Well-typed programs can go very wrong!
-----------------

<br>

Divide-by-zero

Keys missing in Maps

Pattern-match failures

Buffer overflows (!)

Non-termination


Well-typed programs can go very wrong!
-----------------

<br>

Divide-by-zero

Keys missing in Maps

Pattern-match failures

Buffer overflows (!)

Non-termination

Correctness Assertions...


Goal: Algorithmic Software Verification
---------------------------------------

<br>

**Expressive**

Verify *program specific* properties via *domain specific* analysis

<br>

**Automatic**

Rapid feedback to influence *design*, not only post-facto validation
