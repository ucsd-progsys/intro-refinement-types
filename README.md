README
======

This repository has the materials for a 25 minute talk on Refinement Types,
with [LiquidHaskell](https://github.com/ucsd-progsys/liquidhaskell).

For longer versions, you may be interested in:

+[2 Hr Workshop](http://ucsd-progsys.github.io/lh-workshop/)
+[Tutorial](http://ucsd-progsys.github.io/liquidhaskell-tutorial/)


Running LiquidHaskell
---------------------

1. [Try Online][online]
2. [VM Image][]
3. [Build Locally][local]

[online]: (http://ucsd-progsys.github.io/intro-refinement-types)
[local]:(https://github.com/ucsd-progsys/liquidhaskell-tutorial/blob/master/src/01-intro.lhs#L170-L197)
[vm]: http://goto.ucsd.edu/~gridaphobe/LiquidHaskell.ova

Virtual Machine
---------------

This is also very easy, if you can manage the 2Gb download.

**Step 1** Download [this VM image][vm]

**Step 2** Choose your editor. For *emacs* do:

       tar -zxvf liquid-emacs.tgz

and for *Spacemacs* (a great Vim-Emacs hybrid) do:

       tar -zxvf liquid-spacemacs.tgz

**Step 3** Grab the source files from Github.

Build Slides
------------

To build rust-style html (in dist/_site)

     $ stack exec -- make html

To build reveal.js slides (in dist/_slides)

     $ stack exec -- make slides

Edit Slides
-----------

You can modify the following parameters:

1. **Server URL**: change `liquidserver` in `assets/templates/preamble.lhs`


Misc Links
----------

WBL Heaps

+ [HS+DT proof](https://github.com/jstolarek/dep-typed-wbl-heaps-hs/blob/master/src/TwoPassMerge/CombinedProofs.hs#L68)
+ [HS](https://github.com/jstolarek/dep-typed-wbl-heaps-hs/blob/master/src/TwoPassMerge/NoProofs.hs#L96)
+ [HS+Liquid](https://github.com/ucsd-progsys/liquidhaskell/blob/master/tests/pos/WBL.hs#L129)

Insert Sort

+ https://github.com/davidfstr/idris-insertion-sort/tree/master
+ http://www.enseignement.polytechnique.fr/informatique/INF551/TD/TD5/aux/Insert_Sort.v
+ https://github.com/goldfirere/singletons/blob/master/tests/compile-and-dump/InsertionSort/InsertionSortImp.hs


Outline
-------

+ Motivation
  - Bugs
  - Orwell
  - Go wrong

+ Theory
  - Formal language
  - Dependent Application
  - Predicate Subtyping

+ Examples
  * First Order
    - pre/post : at/length
    - checking : sum
    - infer    : sumInfer

  * Higher Order
    - range, sumHO

  * Data types
    - average  
    - map
    - hwAverage
    - insertSort

+ Abstracting Refinements
  - Incrs/Decrs
  - insertSort
  - foldr

+ Current Status
  - LiquidHaskell/RefScript (UCSD)
  - Catalyst (Purdue)
  - DML/Stardust/ATS (CMU/BU)
  - F7/F* (MSR/Inria)
  - RefinedTypedRacket (IU)
  - Synquid (MIT)
