
 {#intro}
=========

 {#firstbug0}
-------------

<img src="../img/firstbug-crop.jpg" height=400px>

The First *Bug*
---------------

<img src="../img/firstbug-crop2.jpg" height=300px>

**Page from Harvard Mark II log**

A dead moth removed from the device


<div class="hidden">

Morris Worm (1988)
------------------

<img src="../img/RobertMorris.png" height=300px>

+ **Buffer overflow** in `fingerd`

+ "Breaks internet" for several days

+ Harmless internet probe gone berserk

</div>

Slammer Worm (2003)
-------------------

<img src="../img/sapphire.gif" height=300px>

**Buffer Overflow**

Affected 90% of vulnerable machines in 10 mins

Northeast Blackout (2003)
-------------------------

<img src="../img/blackout.gif" height=300px>

**Race Condition**

Cut power for 55 million, trigger: lines hitting foliage

HeartBleed (2014)
-----------------

<img src="../img/heartbleed.png" height=300px>

**Buffer Overflow**

Compromises secret keys, passwords ...


Goto Fail (2014)
----------------

<img src="../img/gotofail.png" height=300px>

**Typo (?!)**

Bypass critical check, compromise cryptography

A Possible Solution
===================

 Modern Languages
-----------------

<div class="fragment">

<img src="../img/george-orwell.jpg" height=250px>

</div>

<div class="fragment">

<img src="../img/thoughtcrime.png" height=100px>

</div>

Modern Languages
----------------

<br>

F#

Rust

Scala

OCaml

**Haskell**


Modern Languages
----------------

<br>

Static Typing

<br>

First-class Functions

<br>

Immutability by Default


<br>

<div class="fragment">

Make **good** designs **easy** and **bad** designs **hard**

</div>


Modern Languages?
-------------------

<br>

Not so fast ...

<br>

<div class="fragment">

Well-typed programs **can go wrong!**

</div>


Well-Typed Programs Can Go Wrong
================================

 {#asd}
-------

<div class="hidden">

\begin{code}
main = putStrLn "Easter Egg: to force Makefile"
\end{code}

</div>


Division By Zero
----------------


<div class="fragment">
\begin{spec}
λ> let average xs = sum xs `div` length xs

λ> average [1,2,3]
2
\end{spec}
</div>

<br>

<div class="fragment">
\begin{spec}
λ> average []
*** Exception: divide by zero
\end{spec}

</div>

Missing Keys
------------

<div class="fragment">
\begin{spec}
λ> :m +Data.Map
λ> let m = fromList [ ("haskell", "static")
                    , ("python" , "dynamic")]

λ> m ! "haskell"
"static"
\end{spec}
</div>

<br>

<div class="fragment">
\begin{spec}
λ> m ! "javascript"
"*** Exception: key is not in the map
\end{spec}
</div>

Segmentation Faults
-------------------

<div class="fragment">
\begin{spec}
λ> :m +Data.Vector
λ> let v = fromList ["haskell", "python"]
λ> unsafeIndex v 0
"haskell"
\end{spec}
</div>

<div class="fragment">
<br>
\begin{spec}
λ> V.unsafeIndex v 3


'ghci' terminated by signal SIGSEGV ...
\end{spec}
</div>


"HeartBleeds"
-------------

\begin{spec}
λ> :m + Data.Text Data.Text.Unsafe
λ> let t = pack "Boulder"
λ> takeWord16 4 t
"Boul"
\end{spec}

<br>

<div class="fragment">
Memory overflows **leaking secrets**...

<br>

\begin{spec}
λ> takeWord16 20 t
"Boulder\1912\3148\SOH\NUL\15928\2486\SOH\NUL"
\end{spec}
</div>

Goal: Algorithmic Verification
==============================

 {#algo}
--------

**Expressive**

<div class="fragment">
Verify *programmer specified* properties
</div>

<div class="fragment">
Using *domain specific* analyses
</div>

<br>

**Automatic**

<div class="fragment">
Rapid and automated feedback while programming
</div>

<div class="fragment">
Influence *design*, not only post-facto *validation*
</div>



Tension
-------

<img src="../img/tension.png" height=300px>

Automation vs. Expressiveness

Tension
-------

<img src="../img/tension0.png" height=300px>

Extremes: Coverity vs. CoC

Tension
-------

<img src="../img/tension1.png" height=300px>

Can specify simple properties in modern type systems

Tension
-------

<img src="../img/tension2.png" height=300px>

**Model checkers** trade automation for expressiveness

(require *stubs* or *harnesses*)

Tension
-------

<img src="../img/tension3.png" height=300px>

**Program logics** trade automation for expressiveness

(require *invariants*)


Tension
-------

<img src="../img/tension4.png" height=300px>

**Goal:** Find a sweet spot?

<div class="fragment">
[[continue]](Index.lhs.slides.html#/plan)

</div>


<div class="hidden">

BEGIN CUT

Program Logics
--------------

<br>

**Floyd-Hoare** (ESC, Dafny, SLAM/BLAST,...)

<br>

+ **Properties:**   Assertions & Pre- and Post-conditions

+ **Proofs:**       Verification Conditions proved by SMT

+ **Inference:**    Abstract Interpretation

<br>

<div class="fragment"> Automatic but **not** Expressive </div>


Program Logics
--------------

<br>

**Floyd-Hoare** (ESC, Dafny, SLAM/BLAST,...)

<br>

Automatic but **not** Expressive

<br>

+ Rich Data Types ?

+ Higher-order functions ?

+ Polymorphism ?

Refinement Types
----------------

<br>

Generalize *Program Logics* with *Types*

<br>

+ **Properties:**  Types + Predicates

+ **Proofs:**      Subtyping + SMT Solvers

<div class="hidden">

+ **Inference:**   Hindley-Milner + Abstract Interpretation

</div>

<div class="fragment">
  <br>
  Towards reconciling Automation and Expressiveness
</div>

<br>

<div class="fragment">
[[continue]](01_SimpleRefinements.lhs.slides.html)
</div>

END CUT
</div>
