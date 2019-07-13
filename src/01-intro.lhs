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
<h1 style="border-bottom:none">Verification with Refinement Types</h1>

<br>

<h4 style="border-bottom:none">
<font color="#1569C7">Ranjit Jhala</font>
</h4>

<br>

<h4 style="border-bottom:none"><font color="green">
UC San Diego & AWS/ARG
</font></h4>

</p>

</div>



Follow Along at This URL
------------------------

<br>
<br>
<br>

[http://ranjitjhala.github.io/CAV2019-tutorial/](http://ranjitjhala.github.io/CAV2019-tutorial/)

<br>

**Zoom in** to see nav arrows

Program Analysis & Verification
-------------------------------

<br>
<br>
<br>
<br>

Why limited broader impact and adoption?

Program Analysis & Verification
-------------------------------

Why limited broader impact and adoption?

<p align=center>
<img src="img/intro0.png" height=280px/>
</p>

Program Analysis & Verification
-------------------------------

Why limited broader impact and adoption?

<p align=center>
<img src="img/intro1.png" height=280px/>
</p>


Program*mer's* Analysis & Verification
---------------------------------------

<p align=center>
<img src="img/intro2.png" height=330px/>
</p>

Analysis Influences Program's Design
------------------------------------

<p align=center>
<img src="img/intro-fp0.png" height=330px/>
</p>

Analysis Influences Program's Design
------------------------------------

<p align=center>
<img src="img/intro-fp1.png" height=330px/>
</p>

Analysis Influences Program's Design
------------------------------------

<br>

Uber NullAway [Sridharan et al.](https://eng.uber.com/nullaway/)

<p align=center>
<img src="img/intro-uber.png" height=220px style="-webkit-filter: drop-shadow(5px 5px 5px #222); filter: drop-shadow(5px 5px 5px #222);"/>
</p>

Analysis Influences Program's Design
------------------------------------

<br>

Static Analysis at Google [Sadowski et al. 2018](https://ai.google/research/pubs/pub46576) 

<p align=center>
<img src="img/intro-google.png" height=220px style="-webkit-filter: drop-shadow(5px 5px 5px #222); filter: drop-shadow(5px 5px 5px #222);"/>
</p>

Analysis Influences Program's Design
------------------------------------

<br>

Infer Analysis at Facebook [Calcagno et al. 2018](https://research.fb.com/popl-2019-most-influential-paper-award-for-research-that-led-to-facebook-infer/) 

<p align=center>
<img src="img/intro-fb.png" height=250px style="-webkit-filter: drop-shadow(5px 5px 5px #222); filter: drop-shadow(5px 5px 5px #222);"/>
</p>

Program*mer's* Analysis & Verification
---------------------------------------

<p align=center>
<img src="img/intro2.png" height=330px/>
</p>


Program Influences Analysis' Abilities
--------------------------------------

<p align=center>
<img src="img/intro-out0.png" height=330px/>
</p>

Program Influences Analysis' Abilities
--------------------------------------

<p align=center>
<img src="img/intro-out1.png" height=330px/>
</p>

Language Integrated Verification (LIVE)
---------------------------------------

<p align=center>
<img src="img/intro2.png" height=260px/>
</p>

<br>

This tutorial: **LIVE with Refinement Types**

LIVE with Refinement Types
--------------------------

<br>

**Tutorial Goals**

<br>

Why and how to *use* Refinement Types 

<br>

How to *implement* Refinement Types

Plan
----

<br> 

**Part I:** **[Refinements 101](02-refinements.html)**

Case Study: [Vector Bounds](03-example-vectors.html)

**Part II:** [Properties of Structures](04-data-properties.html)

Case Study: [Sorting](05-example-sort.html), [Interpreter](06-example-interpreter.html)

**Part III:** [Invariants of Data Structures](07-data-legal.html)

Case Study: [Sorting actually Sorts Lists](08-example-sort.html)

**Part IV:** [Termination](09-termination.html) and [Correctness Proofs](10-reflection.html)

Case Study: [Optimizing Arithmetic Expressions](11-example-opt.html)
