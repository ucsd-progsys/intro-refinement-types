<div class="hidden">
\begin{code}
module Intro where

dummy :: Int
dummy = 7
\end{code}
</div>

<section>

<br>
<br>
<br>


<p align=center>
<h1 style="border-bottom:none">Verification and Synthesis with Refinement Types</h1>

<br>

<h4 style="border-bottom:none"><i>Ranjit Jhala (UCSD)</i></h4>
<h4 style="border-bottom:none"><i>Niki Vazou (UMD)</i></h4>
<h4 style="border-bottom:none"><i>Nadia Polikarpova (MIT)</i></h4>

</p>

</section>

Follow Along Here
-----------------

<br>
<br>
<br>

[http://ucsd-progsys.github.io/intro-refinement-types/120/](http://ucsd-progsys.github.io/intro-refinement-types/120/)


Plan
----

[**Motivation**](01-index.html)

**Part I: Refinement Types**

- [**Refinements**](02-refinements.html)
- [**Data Types**](03-datatypes.html)
- [**Case Study: Sorting**](04-case-study-insertsort.html)

**Part II: Reflection**

- [**Termination**](05-termination.html)
- [**Reflection**](06-reflection.html)
- [**Reflection**](07-structural-induction.html)
- [**Case Study: Map-Reduce**](08-case-study-map-reduce.html)


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


Division By Zero
----------------

<br>

\begin{spec}
λ> let average xs = sum xs `div` length xs

λ> average [100, 202, 300]
2
\end{spec}

Division By Zero
----------------

<br>

\begin{spec}
λ> let average xs = sum xs `div` length xs

λ> average [100, 202, 300]
2

λ> average []
*** Exception: divide by zero
\end{spec}


Missing Keys
------------

<br>

\begin{spec}
λ> :m +Data.Map
λ> let m = fromList [ ("haskell"    , "lazy")
                    , ("javascript" , "eager")]
\end{spec}

Missing Keys
------------

<br>

\begin{spec}
λ> :m +Data.Map
λ> let m = fromList [ ("haskell"    , "lazy")
                    , ("javascript" , "eager")]

λ> m ! "haskell"
"lazy"
\end{spec}


Missing Keys
------------

<br>

\begin{spec}
λ> :m +Data.Map
λ> let m = fromList [ ("haskell"    , "lazy")
                    , ("javascript" , "eager")]

λ> m ! "haskell"
"lazy"

λ> m ! "clojure"
"*** Exception: key is not in the map
\end{spec}



Segmentation Faults
-------------------

<br>

\begin{spec}
λ> :m +Data.Vector
λ> let v = fromList ["haskell", "javascript"]

λ> unsafeIndex v 0
"haskell"
\end{spec}

Segmentation Faults
-------------------

<br>

\begin{spec}
λ> :m +Data.Vector
λ> let v = fromList ["haskell", "javascript"]

λ> unsafeIndex v 0
"haskell"

λ> V.unsafeIndex v 3

'ghci' terminated by signal SIGSEGV ...
\end{spec}



HeartBleeds
-----------

<img src="img/heartbleed.png" height=300px>


HeartBleeds
-----------

<br>

\begin{spec}
λ> :m + Data.Text Data.Text.Unsafe
λ> let t = pack "Barcelona"

λ> takeWord16 5 t
"Barce"
\end{spec}


HeartBleeds
-----------

\begin{spec}
λ> :m + Data.Text Data.Text.Unsafe
λ> let t = pack "Barcelona"

λ> takeWord16 5 t
"Barce"
\end{spec}

**Memory overflows leaking secrets...**

\begin{spec}
λ> takeWord16 20 t
"Barcelona\1912\3148\NUL\15928\2486\SOH\NUL"
\end{spec}


Goal: Programmer *Extensible* Analysis
--------------------------------------

<br>
<br>
<br>

To prevent **wider class** of errors

To enforce **program specific** properties

To analyze **during** development (not just validate *after*)


Plan
----

**Part I: Refinement Types**

- [**Refinements**](02-refinements.html)
- [**Data Types**](03-datatypes.html)
- [**Case Study: Sorting**](04-case-study-insertsort.html)

**Part II: Reflection**

- [**Termination**](05-termination.html)
- [**Reflection**](06-reflection.html)
- [**Reflection**](07-structural-induction.html)
- [**Case Study: Map-Reduce**](08-case-study-map-reduce.html)


Evaluation
----------

<br>

+ Diverse Code Bases

+ 10KLoc / 56 Modules

+ Memory Safety, Termination, Functional Correctness

<br>

<div class="fragment">
**Inference is Crucial**
</div>

Evaluation
----------


<img src="img/code-spec-indiv.png" height=250px>

+ **Specifications:** 1 / 10 LOC  (*ok*)

+ **Compile Time:**  1s / 20 LOC  (*not ok!*)


<div class="hidden">

Evaluation
----------


**Library**                     **LOC**     **Specs**      **Time**
---------------------------   ---------   -----------    ----------
`XMonad.StackSet`                   256            74          27s
`Data.List`                         814            46          26s
`Data.Set.Splay`                    149            27          27s
`Data.Vector.Algorithms`           1219            76          61s
`Data.Map.Base`                    1396           125          68s
`Data.Text`                        3128           305         231s
`Data.Bytestring`                  3505           307         136s
**Total**                     **11512**       **977**     **574s**
---------------------------   ---------   -----------    ----------

</div>


Conclusion
----------

<br>

**Refinement Types:** Automated Dependent Typing via SMT

<br>


|                     |                                |
|--------------------:|:-------------------------------|
| **Refinements:**    | Types + Predicates             |
| **Specification:**  | Refined Input/Output Types     |
| **Verification:**   | SMT-based Predicate Subtyping  |
| **Measures:**       | Specify Properties of Data     |
| **Termination:**    | Well-founded Metrics           |
| **Reflection:**     | Haskell functions in Logic     |


Current & Future Work
---------------------

<br>

Faster Checking

Easier Errors

**Code Synthesis**


Thank You!
----------

<br>
<br>


[`http://www.refinement-types.org`](http://www.refinement-types.org)
