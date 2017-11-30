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
<h1 style="border-bottom:none">Liquid Haskell:
<br>Verification of Haskell Code</h1>

<br>

<h4 style="border-bottom:none"><i>Niki Vazou 
<br> (University of Maryland)</i></h4>

</p>

</section>



Liquid Haskell: Verification of Haskell Code
=======================================================

<br>

<p align="center">
**Motivation:** _Why_ verification?
</p>


Liquid Haskell: Verification of Haskell Code
=======================================================

<br>

<p align="center">
**Motivation:** _Why_ verification?
</p>

In class motivation: 
programs may diverge or crash.

< *** Exception: Prelude.head: empty list

<br>

But the goal is more ambitious ...

Software bugs are Everywhere
=============================
<p align="center">
“Airbus A400M crashed due to a software bug.”
</p>
<p align="center">
— May 2015
</p>
<p align="center">
<img src="https://si.wsj.net/public/resources/images/BN-NP763_aircut_P_20160419072444.jpg" alt="Plane" style="width: 300px;" align="middle" />
</p>

Software bugs are Everywhere
=============================
<p align="center">
“[Heartbleed](https://en.wikipedia.org/wiki/Heartbleed): a security bug in the OpenSSL cryptography library.”
</p>
<p align="center">
— April 2014
</p>
<p align="center">
<img src="http://heartbleed.com/heartbleed.png" alt="The Heartbleed Bug." style="height: 260px;" align="middle" />
</p>

How The Heartbleed Bug Works
=============================
<p align="center">
<img src="http://imgs.xkcd.com/comics/heartbleed_explanation.png" alt="How The Heartbleed Bug Works" style="width: 500px;" align="middle" />



How The Heartbleed Bug Works
=============================
<p align="center">
<img src="img/heartbleed2.png" alt="How The Heartbleed Bug Works" style="width: 500px;" align="middle" />


How The Heartbleed Bug Works
=============================
<p align="center">
<img src="img/heartbleed3.png" alt="How The Heartbleed Bug Works" style="width: 500px;" align="middle" />


Goal: Make Bugs Difficult to Express
==================================

<br>

Using Modern Programming Languages (e.g., Haskell, Scala, Ocaml, F#).

<br>

Because of Strong Types & Lambda Calculus.

<br>

Via compile-time sanity checks
=====================================
<p align="center">
<img src="http://goto.ucsd.edu/~nvazou/images/lambda-man.png" alt="Lambda Man." style="height: 260px;" align="middle" />
</p>
<br>

Fact Check: Haskell VS. Heartbleed
=====================================
<p align="center">
<img src="http://goto.ucsd.edu/~nvazou/images/haskellbleed.png" alt="Haskell vs Heartbleed" style="height: 260px;" align="middle" />

<div class="hidden">

\begin{code}
main = putStrLn "Easter Egg: to force Makefile"
\end{code}

</div>




The Heartbleed Bug in Haskell
=====================================

\begin{code}
λ> :m +Data.Text Data.Text.Unsafe
λ> let text = pack "hat"
λ> :t takeWord16
    takeWord16 :: Int -> Text -> Text
\end{code}


`True` is a bad argument 
=====================================

\begin{code}
λ> takeWord16 True text

<interactive>:5:12:
    Couldn't match expected type ‘Int’ with actual type ‘Bool’
    In the first argument of ‘takeWord16’, namely ‘True’
    In the expression: takeWord16 True text
\end{code}


But, `10` is a good argument 
=====================================

<br>
Reveal `7` extra characters...
<br>

\begin{code}
λ>  takeWord16 10 text
"hat\33624\5479\SOH\NUL\60480\5115\5479"
\end{code}


More Bugs: Functional Correctness
=====================================

<br>
\begin{code}
λ> sort  [4,3,2,1]
[4,3,2,1]
\end{code}




More Bugs: Program Equivalence
=====================================

<br>
\begin{code}
λ> sum  [1..1000]
500500

λ> psum [1..1000]
0
\end{code}







Goal: Extend Type System
=====================================

<br>


+ To prevent **wider class** of errors

+ To enforce **program specific** properties



Plan
=====================================

<br>

- [**1. Refinements Types**](02-refinements.html)
- [**2. Data Types**](03-datatypes.html)
- [**3. Case Study: Insert Sort**](04-insert-sort.html)

<br>

Coming up: 

- **4. Termination**
- **5. Proving Laws**
- **6. Natural Deduction**



Installation & Resources
=============

- Online Server: [http://goto.ucsd.edu:8090/index.html](http://goto.ucsd.edu:8090/index.html)

- `cabal install liquidhaskell`

- From [github](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/INSTALL.md)


- Further Reading: [Blog](https://ucsd-progsys.github.io/liquidhaskell-blog/), [Manual](http://ucsd-progsys.github.io/lh-workshop/), [Github](https://github.com/ucsd-progsys/liquidhaskell/issues).


<div class="hidden">


Recap
-----

<br>
<br>

1. **Refinements:** Types + Predicates
2. **Subtyping:** SMT Implication
3. **Measures:** Specify Properties of Data
4. **Termination:** Use Logic to Prove Termination
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

Evaluation
----------

<br>

+ Diverse Code Bases

+ 20KLoc

+ Memory Safety, Termination, Functional Correctness, Program Equivalence

+ **Specifications:** 1 / 10 LOC  

<br>

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


<div class="hidden">

Evaluation
----------


**Library**                     **LOC**     **Specs**      **Time**
---------------------------   ---------   -----------    ----------
`XMonad.StackSet`                   256            74          27s
`Data.List`                         814            46          26s
`Data.Set.Splay`                    149            27          27s
`Data.Vector.Algorithms`           1219            76          89s
`HsColour`                         1047            19         196s
`Data.Map.Base`                    1396           125         174s
`Data.Text`                        3128           305         499s
`Data.Bytestring`                  3505           307         294s
**Total**                     **11512**       **977**    **1336s**
---------------------------   ---------   -----------    ----------

</div>





Conclusion
----------

<br>

**Liquid Types:** Automated Verification via SMT

<br>

<div class="fragment">

-------------------       ------------------------------------------------
**Properties:**           Predicates  *+ Types*
**Proofs:**               SMT Solvers *+ Subtyping*
-------------------       ------------------------------------------------

</div>


<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>





Thank You!
----------

<br>

[https://github.com/ucsd-progsys/liquidhaskell](https://github.com/ucsd-progsys/liquidhaskell)

<br>

[`http://www.refinement-types.org`](http://www.refinement-types.org)

<br>

[online demo @ http://goto.ucsd.edu/liquidhaskell](http://goto.ucsd.edu/liquidhaskell)

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>


</div>