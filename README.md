# README

This repository has the materials for

* a [25 minute talk on Refinement Types][online],
* a [120 minute talk]

on [Refinement Types and LiquidHaskell][lh].

For longer versions, you may be interested in:

+ [2 Hr Workshop](http://ucsd-progsys.github.io/lh-workshop/)
+ [Tutorial](http://ucsd-progsys.github.io/liquidhaskell-tutorial/)

## TODO

- [*] Add `liquid-client` as submodule
- [*] Move to github `docs` style
- [*] Move current to `src-25` which produces talk in `docs/25/`
- [*] Move current to `src-120` which produces talk in `docs/120/`
- [ ] Check that you can build+push *both* `25` and `120` versions
- [ ] Copy stuff from `lh-workshop` over to `120`
- [ ] Edit stuff in `120`

## Running LiquidHaskell

1. [Try Online][online]
2. [VM Image][]
3. [Build Locally][local]

## Virtual Machine

This is also very easy, if you can manage the 2Gb download.

**Step 1** Download [this VM image][vm]

**Step 2** Choose your editor. For *emacs* do:

       tar -zxvf liquid-emacs.tgz

and for *Spacemacs* (a great Vim-Emacs hybrid) do:

       tar -zxvf liquid-spacemacs.tgz

**Step 3** Grab the source files from Github.

## Build Slides

To build rust-style html (in dist/_site)

     $ stack exec -- make html

To build reveal.js slides (in dist/_slides)

     $ stack exec -- make slides

## Edit Slides

You can modify the following parameters:

1. **Server URL**: change `liquidserver` in `assets/templates/preamble.lhs`
2. **MathJax URL**: change the relevant link in `assets/templates/pagemeta.template`

## Outline [25]

+ 01-intro         [3]
+ 02-refinements   [6]
+ 03-examples      [9]
+ 04-abstracting   [4]
+ 05-concl         [3]


## Misc Links

WBL Heaps

+ [HS+DT proof](https://github.com/jstolarek/dep-typed-wbl-heaps-hs/blob/master/src/TwoPassMerge/CombinedProofs.hs#L68)
+ [HS](https://github.com/jstolarek/dep-typed-wbl-heaps-hs/blob/master/src/TwoPassMerge/NoProofs.hs#L96)
+ [HS+Liquid](https://github.com/ucsd-progsys/liquidhaskell/blob/master/tests/pos/WBL.hs#L129)

Insert Sort

+ https://github.com/davidfstr/idris-insertion-sort/tree/master
+ http://www.enseignement.polytechnique.fr/informatique/INF551/TD/TD5/aux/Insert_Sort.v
+ https://github.com/goldfirere/singletons/blob/master/tests/compile-and-dump/InsertionSort/InsertionSortImp.hs

[online]: http://ucsd-progsys.github.io/intro-refinement-types
[local]:  https://github.com/ucsd-progsys/liquidhaskell-tutorial/blob/master/src/01-intro.lhs#L170-L197
[vm]:     http://goto.ucsd.edu/~gridaphobe/LiquidHaskell.ova
[lh]:     https://github.com/ucsd-progsys/liquidhaskell
