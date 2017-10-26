<div class="hidden">
\begin{code}
module Intro where

dummy :: Int
dummy = 7
\end{code}
</div>

<section>

<br>



<p align=center>
<h1 style="border-bottom:none">Refinement Reflection: 
<br> Complete Verification with SMT</h1>

<br>
<br>

<h4 style="border-bottom:none"><i>Niki Vazou 
<br> (Univerisyt of Maryland)</i></h4>

</p>
<br>
<br>
<br>

</section>

Two Tends in Program Verification  
-----------------

<br>

- Type-Theory-based theorem provers (*e.g.,* Coq and Agda)


<br>

- SMT-based verifiers (*e.g.,* Dafny and F*)

<br>


Type-Theory-based theorem provers 
----------------------------------

<br>

- Type level computation to reason about program properties.

- Supply lemmata or rewrite hints to discharge proofs, ... 

- ... even in decidable logics.



SMT-based verifiers
----------------------------------
<br>

- Automate the verification of programs over decidable theories.

- Encode user-defined functions with universally-quantified axioms, ... 

- ... using incomplete (*i.e.,* **unpredictable**) heuristics for instantiation.



Predictable & SMT-automated Verification
------------------------------------------

<br> 
<br>


**GOAL:** Alternative representation of user-defined functions ...


... for SMT-decidable (*i.e.,* **unpredictable**) verification.

<br>

Refinement Reflection 
---------------------

<br>

Reflect function definition into its result refined type.

**Consequence:** 

Value level function application is also ...

... "SMT" **predictable** function instantiation.


Outline
---------------------

<br>

How far can we go?

- [**1. Refinement Reflection: Fibonacci**](02-reflection.html)
- [**2. Case Study: MapReduce Equivalence**](03-map-reduce.html)
- [**3. Encoding Natural Deduction**](04-natural-deduction.html)


Evaluation
-----------

<img src="img/benchmarks.png" height=250px>

Conclusion
------------

<br> 

- Refinement Reflection and Proof by Logical Evaluation combined ...

- ... allow for complete verification with SMT-automation, but

- ... don't allow for user interaction,

- ... don't allow for proof certificates.



Thank you! 
-----------

<br>

[Refinement Reflection: Complete Verification with SMT](https://nikivazou.github.io/static/popl18/refinement-reflection.pdf)

to appear in POPL 2018

- *by* Niki Vazou, Anish Tondwalker, Vikraman Choudhoury, 
- Ryan Scott, Ryan Newton, Philip Wadler, and Ranjit Jhala
