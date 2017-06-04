
All the Code
------------


\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}

module IntroToRefinemetntTypes where

import Prelude hiding (length, (!))

import Data.Vector ((!), length)

vectorSum v  = loop 0
  where
    n        = length v
    loop i   = if i < n then (v ! i) + loop (i + 1)
                        else 0
\end{code}



Slide 1
-------

<a href="http://www.google.com">Slide 1</a>

This is the _first-est_ **slide**

Slide 2
-------

<a href="http://www.bing.com">Slide 2</a>

This is the _second_ **slide**

Slide 3
-------

<a href="http://www.nytimes.com">Slide 3</a>

This is the _third_ **slide**

Slide 4: Integers equal to `101`
------------------------------

<br>

\begin{code}
{-@ lestats :: {v:Int | v == 101} @-}
lestats :: Int
lestats = 101
\end{code}
<br>


Slide 5: Integers equal to `56`
------------------------------

<br>

\begin{code}
{-@ chips :: {v:Int | v == 101} @-}
chips :: Int
chips = 56
\end{code}


<br>
