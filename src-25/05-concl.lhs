
LiquidHaskell
-------------

<br>

LiquidHaskell
-------------

<br>

Diverse Code Bases

10KLoc / 56 Modules


LiquidHaskell
-------------

<br>

Memory Safety

Totality

Termination

Functional Correctness

<div class="fragment">
**Inference is Crucial**
</div>

Evaluation
----------


<img src="img/code-spec-indiv.png" height=200px>

**Specifications:** 1 / 10 LOC  (*ok*)

**Analysis Time:**  1s / 10 LOC  (*not ok!*)


Recap: Refinement Types
-----------------------

<br>

**Expressive**

Verify *program specific* properties via *domain specific* analysis

<br>

**Automatic**

Rapid feedback to *influence design*, not only post-facto validation


Recap: Refinement Types
-----------------------

<br>

<img src="img/tension3.png" height=200px>


Recap: Refinement Types
-----------------------

<br>

<img src="img/tension5.png" height=200px>


Recap: Refinement Types
-----------------------

<br>

**SMT-Automated Dependent Typing**

<div class="fragment">

-------------------       -------------------------------------
**Properties:**           Predicates  *+ Types*
**Proofs:**               SMT Solvers *+ Subtyping*
**Inference:**            Abstr. Interp. *+ Hindley-Milner*
-------------------       -------------------------------------

</div>

Many Friends, Many Directions
-----------------------------

<br>

----------------------------    ------------
[ATS][ats]                      BU
[Catalyst][catalyst]            Purdue
[DML/Stardust][stardust]        CMU/UBC
[F7/F*][fstar]                  MSR/INRIA
[RefinedRacket][rtr]            IU
[Synquid][synquid]              MIT
----------------------------    ------------


Thank You!
----------

<br>
<br>
<br>

<h4 style="border-bottom:none">
[`http://www.refinement-types.org`](http://www.refinement-types.org)
</h4>



[lh]: http://www.refinement-types.org
[rsc]: http://www.refinement-types.org
[stardust]: http://www.mpi-sws.org/~joshua/type-refinements.info/
[ats]: https://github.com/ats-lang/ats-lang.github.io
[fstar]: http://fstar-lang.org
[catalyst]: http://gowthamk.github.io/docs/icfp77-kaki.pdf
[rtr]: http://arxiv.org/abs/1511.07033
[synquid]: http://arxiv.org/pdf/1510.08419.pdf
