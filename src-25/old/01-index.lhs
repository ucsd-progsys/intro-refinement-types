
<div class="slide">

<br>
<br>
<br>
<br>

<h1 style="border-bottom:none">Programming with Refinement Types</h1>

<h4 style="border-bottom:none"><i>Ranjit Jhala (University of California, San Diego)</i></h4>

</div>

Well-Typed Programs *Can* Go Wrong
==================================


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

λ> average [100, 202, 300]
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
λ> let m = fromList [ ("haskell"    , "lazy")
                    , ("javascript" , "eager")]

λ> m ! "haskell"
"lazy"
\end{spec}
</div>

<br>

<div class="fragment">
\begin{spec}
λ> m ! "clojure"
"*** Exception: key is not in the map
\end{spec}
</div>


Segmentation Faults
-------------------

<div class="fragment">
\begin{spec}
λ> :m +Data.Vector
λ> let v = fromList ["haskell", "javascript"]
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
λ> let t = pack "StrangeLoop"
λ> takeWord16 5 t
"Stran"
\end{spec}

<br>

<div class="fragment">
Memory overflows **leaking secrets**...

<br>

\begin{spec}
λ> takeWord16 20 t
"StrangeLoop\1912\3148\NUL\15928\2486\SOH\NUL"
\end{spec}
</div>



Goal: Extend Type System
------------------------

<br>

<br>

+ To prevent **wider class** of errors

+ To enforce **program specific** properties


Plan
----

<br>

**Part I: Refinement Types**

+ <div class="fragment"> [**Refinements**](02-refinements.html)</div>
+ <div class="fragment"> [**Data Types**](03-datatypes.html)</div>

<br>

**Part II: Case Studies**

+ <div class="fragment">[**Insertion Sort**](04-case-study-insertsort.html)</div>
+ <div class="fragment">[**Well Scoped Evaluator**](05-case-study-eval.html)</div>
+ <div class="fragment">[**Low-level Memory**](06-case-study-bytestring.html)</div>


Evaluation
----------

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

**Refinement Types:** Automated Dependent Typing via SMT

<br>

<div class="fragment">

-------------------       ------------------------------------------------
**Properties:**           Predicates  *+ Types*
**Proofs:**               SMT Solvers *+ Subtyping*
**Inference:**            Abstract Interpretation *+ Hindley-Milner*
-------------------       ------------------------------------------------

</div>


Current & Future Work
---------------------

<br>

+ GHC Integration
+ Faster Checking
+ Easier Errors
+ Code Synthesis



Thank You!
----------

<br>
<br>


[`http://www.refinement-types.org`](http://www.refinement-types.org)
