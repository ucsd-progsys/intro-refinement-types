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

<h4 style="border-bottom:none"><i>Ranjit Jhala (UCSD)</i></h4>
</p>

</div>


Follow Along Here
-----------------

<br>
<br>
<br>

[http://ucsd-progsys.github.io/intro-refinement-types/](http://ucsd-progsys.github.io/intro-refinement-types/25/)


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

Goal: Algorithmic Software Verification
---------------------------------------

<br>

**Expressive**

Verify *program specific* properties via *domain specific* analysis

Goal: Algorithmic Software Verification
---------------------------------------

<br>

**Expressive**

Verify *program specific* properties via *domain specific* analysis

<br>

**Automatic**

Rapid feedback to *influence design*, not only post-facto validation

Outline
-------

<br>

[Motivation](01-intro.html)

Outline
-------

<br>

[Motivation](01-intro.html)

[Refinements 101](02-refinements.html)

Outline
-------

<br>

[Motivation](01-intro.html)

[Refinements 101](02-refinements.html)

[Refinements by Example](03-examples.html)

Outline
-------

<br>

[Motivation](01-intro.html)

[Refinements 101](02-refinements.html)

[Refinements by Example](03-examples.html)

[Abstracting Over Refinements](04-abstracting.html)

Outline
-------

<br>

[Motivation](01-intro.html)

[Refinements 101](02-refinements.html)

[Refinements by Example](03-examples.html)

[Abstracting Over Refinements](04-abstracting.html)

[Current Status & Future Directions](05-concl.html)
