---
title: "Habits"
---

<div class="hidden">
\begin{code}
module Intro where

dummy :: Int
dummy = 7
\end{code}
</div>

<div class="slide">

<br>
<br>
<br>

<p align=center>
<h1 style="border-bottom:none">An Introduction to Refinement Types</h1>

<br>

<h4 style="border-bottom:none">
<font color="#1569C7">Ranjit Jhala</font>
</h4>

<br>

<h4 style="border-bottom:none"><font color="green">
UC San Diego
</font></h4>

</p>

</div>



Follow Along at This URL
------------------------

<br>
<br>
<br>

[http://ranjitjhala.github.io/pliss21-tutorial/](http://ranjitjhala.github.io/pliss21-tutorial/)

<br>

**Zoom in** to see nav arrows


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

Program Analysis & Verification
-------------------------------

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

<hr style="height:5px; visibility:hidden;" />

Divide-by-zero

Keys missing in Maps

Pattern-match failures


Well-typed programs can go very wrong!
-----------------

<hr style="height:5px; visibility:hidden;" />

Divide-by-zero

Keys missing in Maps

Pattern-match failures

Buffer overflows (!)

Non-termination

Functional Correctness / Assertions...

Goal: Language-Integrated Verification (LIVE)
---------------------------------------------

<br>

**Expressive**

Verify *program specific* properties via *domain specific* analysis

Goal: Language-Integrated Verification (LIVE)
---------------------------------------------

<br>

**Expressive**

Verify *program specific* properties via *domain specific* analysis

<br>

**Automatic**

Rapid feedback to *influence design*, not only post-facto validation


LIVE with Refinement Types
--------------------------

<br> 

**Part I:** **[Refinements on Functions](02-refinements.html)**

Example: [Vector Bounds](03-example-vectors.html)

**Part II:** [Refinements on Datatypes](04-data-properties.html)

Example: [Sorting](06-example-sort.html)

**Part III:** Case Studies

[Use-def analysis](07-example-interpreter.html)

[Pointer Arithmetic](08-example-bytestring.html)