
<div class="hidden">

\begin{code}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--short-names"    @-}
{-@ LIQUID "--prune-unsorted" @-}
{- LIQUID "--diffcheck"     @-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Memory where

import Prelude hiding (null)
import Data.Char
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Data.ByteString.Internal (c2w, w2c)
import Language.Haskell.Liquid.Prelude
\end{code}

</div>

<br>

Case Study: Low Level Memory Safety
============================


<br>


"HeartBleed" in Haskell
-----------------------

<br>

**Modern languages are built on top of C**

<br>

<div class="fragment">
Implementation errors could open up vulnerabilities
</div>

<br>



"HeartBleed" in Haskell (1/3)
-----------------------------

<br>

**A String Truncation Function**

<br>

<div class="hidden">
\begin{spec}
                import Data.ByteString.Char8  (pack, unpack)
                import Data.ByteString.Unsafe (unsafeTake)
\end{spec}
</div>

\begin{spec}
      chop     :: String -> Int -> String
      chop s n = s'
        where
          b    = pack s         -- convert String to low-level
          b'   = unsafeTake n b -- take prefix of n bytes
          s'   = unpack b'      -- convert low-level to String
\end{spec}

<br>


"HeartBleed" in Haskell (2/3)
-----------------------------

<img src="img/overflow.png" height=100px>

Works if you use the **valid prefix** size

\begin{spec}
              λ> let ex = "Ranjit Loves Burritos"
                  
              λ> heartBleed ex 10
              "Ranjit Lov"
\end{spec}

<br>


"HeartBleed" in Haskell (3/3)
-----------------------------

<img src="img/overflow.png" height=100px>

Leaks *overflow buffer* if **invalid prefix** size!

\begin{spec}
              λ> let ex = "Ranjit Loves Burritos"

              λ> heartBleed ex 30
              "Ranjit Loves Burritos\NUL\201\&1j\DC3\SOH\NUL"
\end{spec}


Types Against Overflows
-----------------------

<br>

**Strategy: Specify and Verify Types for**

<br>

1. <div class="fragment">Low-level `Pointer` API</div>
2. <div class="fragment">Lib-level `ByteString` API</div>
3. <div class="fragment">User-level `Application` API</div>

<br>

<div class="fragment">Errors at *each* level are prevented by types at *lower* levels</div>


1. Low-level Pointer API
------------------------

<br>

**Strategy: Specify and Verify Types for**

<br>

1. <font color="#1569C7">Low-level `Pointer` API</font>
2. Lib-level `ByteString` API
3. User-level `Application` API

<br>

Errors at *each* level are prevented by types at *lower* levels


API: Types
----------

<br>

**Low-level Pointers**

`data Ptr a`



API: Types
----------

<br>

**Low-level Pointers**

`data Ptr a`

**Foreign Pointers**

`data ForeignPtr a`

`ForeignPtr` wraps around `Ptr`; can be exported to/from C via FFI

API: Operations (1/2)
---------------------

<div class="mybreak"><br></div>

**Read**

\begin{spec}<div/>
                    peek     :: Ptr a -> IO a
\end{spec}


API: Operations (1/2)
---------------------

<div class="mybreak"><br></div>

**Read**

\begin{spec}<div/>
                    peek     :: Ptr a -> IO a
\end{spec}

**Write**

\begin{spec}<div/>
                    poke     :: Ptr a -> a -> IO ()
\end{spec}



API: Operations (1/2)
---------------------

<div class="mybreak"><br></div>

**Read**

\begin{spec}<div/>
                    peek     :: Ptr a -> IO a
\end{spec}

**Write**

\begin{spec}<div/>
                    poke     :: Ptr a -> a -> IO ()
\end{spec}

**Arithmetic**

\begin{spec}<div/>
                    plusPtr  :: Ptr a -> Int -> Ptr a
\end{spec}

API: Operations (2/2)
---------------------

<br>

**Create**

\begin{spec}
                    malloc  :: Int -> ForeignPtr a
\end{spec}

`malloc n` returns a pointer to a new region with `n` bytes


API: Operations (2/2)
---------------------

<br>

**Create**

\begin{spec}
                    malloc  :: Int -> ForeignPtr a
\end{spec}

<br>

**Unwrap and Use**

\begin{spec}
            withForeignPtr :: ForeignPtr a    -- pointer
                           -> (Ptr a -> IO b) -- action
                           -> IO b            -- result
\end{spec}



Example
-------

<br>

**Allocate a block and write 4 zeros into it**

<div class="mybreak"><br></div>

\begin{code}
zero4 = do fp <- malloc 4
           withForeignPtr fp $ \p -> do
             poke (p `plusPtr` 0) zero
             poke (p `plusPtr` 1) zero
             poke (p `plusPtr` 2) zero
             poke (p `plusPtr` 3) zero
           return fp
        where
           zero = 0 :: Word8
\end{code}

Example
-------

<br>

How to **prevent overflows** e.g. writing 5 or 50 zeros into 4-byte block ?

<div class="mybreak"><br></div>

Example
-------

<br>

How to **prevent overflows** e.g. writing 5 or 50 zeros into 4-byte block ?

<div class="mybreak"><br></div>

**Step 1**

*Refine pointers* with allocated size

Example
-------

<br>

How to **prevent overflows** e.g. writing 5 or 50 zeros into 4-byte block ?

<div class="mybreak"><br></div>

**Step 1**

*Refine pointers* with allocated size

<div class="mybreak"><br></div>

**Step 2**

*Track sizes* in pointer operations

Refined API: Types
------------------

<br>

**1. Refine pointers with allocated size**

\begin{spec}
                measure plen  :: Ptr a -> Int
                measure fplen :: ForeignPtr a -> Int
\end{spec}

Refined API: Types
------------------

<br>

**1. Refine pointers with allocated size**

\begin{spec}
                measure plen  :: Ptr a -> Int
                measure fplen :: ForeignPtr a -> Int
\end{spec}

<div class="mybreak"><br></div>

**Abbreviations for pointers of size `N`**

\begin{spec}
            type PtrN a N        = {v:_ | plen v  = N}
            type ForeignPtrN a N = {v:_ | fplen v = N}
\end{spec}

Refined API: Ops (1/3)
----------------------

<br>

**Create**

\begin{spec}
                    malloc  :: n:Nat -> ForeignPtrN a n
\end{spec}

`malloc n` returns a `ForeignPtr` to a region of size `n`


Refined API: Ops (1/3)
----------------------

<br>

**Create**

\begin{spec}
                    malloc  :: n:Nat -> ForeignPtrN a n
\end{spec}

**Unwrap and Use**

\begin{spec}
      withForeignPtr :: fp:ForeignPtr a             -- pointer
                     -> (PtrN a (fplen fp) -> IO b) -- action
                     -> IO b                        -- use
\end{spec}

Refined API: Ops (2/3)
----------------------

<br>

**Arithmetic**

Refine type to track *remaining* buffer size

\begin{spec}
          plusPtr :: p:Ptr a
                  -> o:{Nat | o <= plen p}   -- offset in bounds
                  -> PtrN b {plen b - o}     -- remainder
\end{spec}

Refined API: Ops (3/3)
----------------------

<br>
<br>
<br>

**Read & Write require non-empty remaining buffer**


Refined API: Ops (3/3)
----------------------

<div class="mybreak"><br></div>

**Read & Write require non-empty remaining buffer**

<div class="mybreak"><br></div>

**Read**

\begin{spec}
                peek :: {v:Ptr a | 0 < plen v} -> IO a
\end{spec}

**Write**

\begin{spec}
             poke :: {v:Ptr a | 0 < plen v} -> a -> IO ()
\end{spec}


Example: Overflow Prevented
---------------------------

<br>

How to *prevent overflows* e.g. writing 5 or 50 zeros?

\begin{code}
exBad = do fp <- malloc 4
           withForeignPtr fp $ \p -> do
             poke (p `plusPtr` 0) zero
             poke (p `plusPtr` 1) zero
             poke (p `plusPtr` 2) zero
             poke (p `plusPtr` 5) zero
           return fp
        where
           zero = 0 :: Word8
\end{code}


2. ByteString API
-----------------


<br>

Strategy: Specify and Verify Types for

<br>

1. Low-level `Pointer` API
2. <font color="#1569C7">Lib-level `ByteString` API</font>
3. User-level `Application` API

<br>

Errors at *each* level are prevented by types at *lower* levels

<br>

ByteString Type
---------------

<div class="mybreak"><br></div>

<img src="img/bytestring.png" height=125px>

\begin{code}
data ByteString = PS 
  { bPtr :: ForeignPtr Word8  -- ^ base pointer
  , bOff :: !Int              -- ^ offset at which string starts
  , bLen :: !Int              -- ^ length of string (from offset)
  }
\end{code}


Refined ByteString Type
-----------------------

<div class="mybreak"><br></div>

<img src="img/bytestring.png" height=125px>

\begin{code}
{-@ data ByteString = PS
      { bPtr :: ForeignPtr Word8
      , bOff :: {v:Nat| v        <= fplen bPtr}
      , bLen :: {v:Nat| bOff + v <= fplen bPtr}
      }                                       
  @-}
\end{code}

**Legal `ByteString`: `bOff` and `bOff + bLen` are within `bPtr` bounds** 

Refined ByteString Type
-----------------------

<div class="mybreak"><br></div>

<img src="img/bytestring.png" height=125px>

<div class="mybreak"><br></div>

**A Useful Abbreviation**

\begin{spec}
            type ByteStringN N = {v:ByteString| bLen v == N}
\end{spec}

`ByteStringN k` is a `ByteString` of size `k`

<div class="hidden">
\begin{code}
{-@ type ByteStringN N = {v:ByteString | bLen v = N} @-}
\end{code}
</div>




Legal Bytestrings
-----------------

<div class="mybreak"><br></div>

\begin{code}
{-@ good1 :: IO (ByteStringN 5) @-}
good1 = do 
  fp <- malloc 5
  return (PS fp 0 5)

{-@ good2 :: IO (ByteStringN 3) @-}
good2 = do 
  fp <- malloc 5
  return (PS fp 2 3)
\end{code}

<div class="mybreak"><br></div>

<div class="fragment">
**Note:** *length* of `good2` is `3` which is *less than* allocated size `5`


Illegal Bytestrings
-----------------

<div class="mybreak"><br></div>

\begin{code}
bad1 = do 
  fp <- malloc 3
  return (PS fp 0 10)

bad2 = do 
  fp <- malloc 3
  return (PS fp 2 2)
\end{code}

<div class="mybreak"><br></div>

Claimed length *exceeds* allocation ... **rejected** at compile time



API `create` : Allocate and Fill a `ByteString`
-----------------------------------------------

<div class="hidden">
\begin{code}
create :: Int -> (Ptr Word8 -> IO ()) -> ByteString
\end{code}
</div>

<div class="mybreak"><br></div>

**Specification**

\begin{code}
{-@ create :: n:Nat -> (PtrN Word8 n -> IO ()) -> ByteStringN n   @-}
\end{code}

**Implementation**

\begin{code}
create n fill = unsafePerformIO $ do
  fp  <- malloc n            -- allocate region
  withForeignPtr fp fill     -- fill it
  return (PS fp 0 n)         -- wrap and return as BS
\end{code}




API: `pack` List of `Char` into `ByteString`
------------

<div class="mybreak"><br></div>

**Specification**

\begin{code}
{-@ pack :: str:[Char] -> ByteStringN (len str) @-}
\end{code}

**Implementation**

\begin{code}
pack str         = create n (\p -> write p xs)
  where
  n              = length str
  xs             = map c2w str
  write p (x:xs) = poke p x >> write (plusPtr p 1) xs
  write _ []     = return  ()
\end{code}


API `unsafeTake` a *prefix* of size `n`
---------------------------------------

<div class="mybreak"><br></div>

**Specification**

\begin{code}
{-@ unsafeTake :: n:Nat -> b:{ByteString| n <= bLen b} -> ByteStringN n @-}
\end{code}

**Implementation**

\begin{code}
unsafeTake n (PS x s l) = PS x s n
\end{code}

<div class="mybreak"><br></div>

**Type ensures client cannot read past buffer**

API `unpack` : Convert `ByteString` into List of `Char`
-------------------------------------------------------

<br>

**Specification**

\begin{spec}
          unpack :: b:ByteString -> StringN (bLen b)
\end{spec}

<br>

<div class="fragment">
**Implementation**

\begin{spec}
          unpack b = skip . for . lack . of . time
\end{spec}
</div>

<div class="hidden">
\begin{code}
{-@ assume unpack :: b:ByteString -> StringN (bLen b) @-}
unpack :: ByteString -> String
unpack _ = undefined 
\end{code}
</div>



3. Application API
------------------

<br>

**Strategy: Specify and Verify Types for**

<br>

1. Low-level `Pointer` API
2. Lib-level `ByteString` API
3. <font color="#1569C7">User-level `Application` API</font>

<br>

**Errors at *each* level are prevented by types at *lower* levels**

Revisit "HeartBleed"
--------------------

<br>

Lets revisit our potentially "bleeding" `chop`


<div class="hidden">
\begin{code}
{-@ type StringN N = {v:String| len v == N} @-}
\end{code}
</div>

\begin{code}
{-@ chop :: s:String -> n:Nat -> StringN n @-}
chop s n = s'
  where
    b    = pack s            -- convert String to low-level
    b'   = unsafeTake n b    -- take prefix of n bytes
    s'   = unpack b'         -- convert low-level to String
\end{code}

**Oops, can you fix the error?**


"HeartBleed" no more
--------------------

<br>

\begin{code}
demo     = [ex6, ex30]
  where
    ex   = "Ranjit likes burritos"  -- has size 21 
    ex6  = chop ex 6                -- ok
    ex30 = chop ex 30               -- out of bounds!
\end{code}

<br>

"Bleeding" `chop ex 30` *rejected* by compiler

Recap: Types vs Overflows
-------------------------

<br>

**Strategy: Specify and Verify Types for**

<br>

1. Low-level `Pointer` API
2. Lib-level `ByteString` API
3. User-level `Application` API

<br>

**Errors at *each* level are prevented by types at *lower* levels**

[Continue...](00-plan.html)


<div class="hidden">
\begin{code}
{-@ invariant {v:ByteString   | bLen  v >= 0} @-}

{-@ qualif PLLen(v:a, p:b) : (len v) <= (plen p) @-}

{-@ assume mallocForeignPtrBytes :: n:Nat -> IO (ForeignPtrN a n) @-}
{-@ type ForeignPtrN a N = {v:ForeignPtr a | fplen v = N} @-}

{-@ malloc :: n:Nat -> IO (ForeignPtrN a n) @-}
malloc = mallocForeignPtrBytes

{-@ assume c_memcpy :: dst:(PtrV Word8)
                    -> src:(PtrV Word8)
                    -> size: {v:CSize | (v <= (plen src) && v <= (plen dst))}
                    -> IO (Ptr Word8)
  @-}
foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

{-@ memcpy :: dst:(PtrV Word8)
           -> src:(PtrV Word8)
           -> size: {v:CSize | (v <= (plen src) && v <= (plen dst))}
           -> IO ()
  @-}
memcpy :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
memcpy p q s = c_memcpy p q s >> return ()
\end{code}

</div>
