
<div class="hidden">
\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}


module IntroToRefinementTypes
       ( sum
       , sumBad
       , sumInfer
       , sumHO
       , average
       , insertSort
       )
       where

import Prelude hiding (foldr, map, sum, length, (!))
import qualified Data.Vector as V
\end{code}
</div>

<div class="slide">

<br>
<br>
<br>
<br>


<p align=center>
<h1 style="border-bottom:none">An Introduction to Refinement Types</h1>

<h4 style="border-bottom:none"><i>Ranjit Jhala (UCSD)</i></h4>
</p>

</div>


Whats this?
-----------

<br>


<p align=center>
<img src="img/firstbug-crop.jpg" height=300px/>
</p>

The First *Bug*
---------------

<br>

<p align=center>
<img src="img/firstbug-crop2.jpg" height=300px/>
</p>

<p align=center>
**Page from Harvard Mark II log**

A *dead moth* removed from the device
</p>


Fast forward to Present Day
---------------------------

<br>

<p align=center>
<img src="img/news-bug-1.png" height=300px>
</p>

Fast forward to Present Day
---------------------------

<br>

<p align=center>
<img src="img/news-bug-2.png" height=325px>
</p>

Fast forward to Present Day
---------------------------

<br>

<p align=center>
<img src="img/news-bug-3.png" height=350px>
</p>

What do you *do* Ranjit?
------------------------

<br>

PL Research
-----------

<br>

<p align=center>
  <img src="img/george-orwell.jpg" height=250px>
</p>

<br>

<img src="img/thoughtcrime.png" height=75px>
