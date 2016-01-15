
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
<img src="img/firstbug-crop.jpg" height=200px/>
</p>

The First *Bug*
---------------

<br>

<p align=center>
<img src="img/firstbug-crop2.jpg" height=120px/>
</p>

<p align=center>
**Page from Harvard Mark II log**

A *dead moth* removed from the device
</p>


Fast forward to Present Day
---------------------------


<p align=center>
<img src="img/news-bug-1.png" height=200px>
</p>

Fast forward to Present Day
---------------------------


<p align=center>
<img src="img/news-bug-2.png" height=225px>
</p>

Fast forward to Present Day
---------------------------


<p align=center>
<img src="img/news-bug-3.png" height=250px>
</p>


Programming Languages Research
------------------------------

<p align=center>
  <img src="img/george-orwell.jpg" height=170px>

  **George Orwell (1984)**
</p>

<img src="img/thoughtcrime.png" height=45px>



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

First-class Functions

Immutability by Default

Modern Languages
----------------

<br>

Static Typing

First-class Functions

Immutability by Default

<br>

Make **good** designs **easy** and **bad** designs **hard**

Modern Languages?
-----------------

<br>
<br>
<br>

**Alas ... well-typed programs go very wrong!**

Well-typed programs can go very wrong!
-----------------

<br>

Well-typed programs can go very wrong!
-----------------

<br>

Divide-by-zero

Keys missing in Maps

Pattern-match failures


Well-typed programs can go very wrong!
-----------------

<hr style="height:30pt; visibility:hidden;" />

Divide-by-zero

Keys missing in Maps

Pattern-match failures

Buffer overflows (!)

Non-termination

Correctness Assertions...


Well-typed programs can go very wrong!
-----------------

<br>

-------------------------         ---------------------------
Divide-by-zero                    Buffer overflows (!)

Keys missing in Maps              Non-termination

Pattern-match failures            Correctness Assertions...
-------------------------         ---------------------------


Goal: Algorithmic Software Verification
---------------------------------------

<br>

**Expressive**

Verify **program specific** properties via *domain specific* analysis

Goal: Algorithmic Software Verification
---------------------------------------

<br>

<h4 style="border-bottom:none">Expressive</h4>

Verify **program specific** properties via *domain specific* analysis

<br>

<h4 style="border-bottom:none">Automatic</h4>

Rapid feedback to **influence design**, not only post-facto validation
