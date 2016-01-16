
LiquidHaskell
-------------

<br>

+ Diverse Code Bases

+ 10KLoc / 56 Modules

+ Memory Safety, Functional Correctness, Termination

<br>

<div class="fragment">
**Inference is Crucial**
</div>

Evaluation
----------


<img src="img/code-spec-indiv.png" height=250px>

+ **Specifications:** 1 / 10 LOC  (*ok*)

+ **Compile Time:**  1s / 10 LOC  (*not ok!*)


Recap: Refinement Types
-----------------------

<br>

**Refinement Types:** Automated Dependent Typing via SMT

<br>

<div class="fragment">

-------------------       ------------------------------------------------
**Properties:**           Predicates  *+ Types*
**Proofs:**               SMT Solvers *+ Subtyping*
**Inference:**            Abstract Interpretation *+ Hindley-Milner*
-------------------       ------------------------------------------------

</div>

Many Friends, Many Directions
-----------------------------

----------------------------    ------------
[Catalyst][catalyst]            Purdue
[DML/Stardust][stardust]        CMU/UBC
[ATS][ats]                      BU
[F7/F*][fstar]                  MSR/Inria
[RefinedTypedRacket][rtr]       IU
[Synquid][synquid]              MIT
----------------------------    ------------


Many Friends, Many Directions
-----------------------------

- [Catalyst][catalyst] @ Purdue
- [DML/Stardust][stardust] @ CMU/UBC
- [ATS][ats] @ BU
- [F7/F*][fstar] @ MSR/Inria
- [RefinedTypedRacket][rtr] @ IU
- [Synquid][synquid] @ MIT

Thank You!
----------

<br>
<br>


[`http://www.refinement-types.org`](http://www.refinement-types.org)


[lh]: http://www.refinement-types.org
[rsc]: http://www.refinement-types.org
[stardust]: http://www.mpi-sws.org/~joshua/type-refinements.info/
[ats]: https://github.com/ats-lang/ats-lang.github.io
[fstar]: http://fstar-lang.org
[catalyst]: http://gowthamk.github.io/docs/icfp77-kaki.pdf
[rtr]: http://arxiv.org/abs/1511.07033
[synquid]: http://arxiv.org/pdf/1510.08419.pdf
