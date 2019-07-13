
\begin{code}

{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple"        @-}
{-@ infixr ++             @-}  -- TODO: Silly to have to rewrite this annotation!

module Language.ConcreteLH.Chapters.Ch03 where

import           Prelude hiding ((++), const, max, or)
import           Language.ConcreteLH.Lib.ProofCombinators
import           Language.ConcreteLH.Lib.Lists
import qualified Language.ConcreteLH.Lib.State as S 

--------------------------------------------------------------------------------
-- | Section 3.1 
--------------------------------------------------------------------------------

type Vname = String

data AExp  
  = N Val 
  | V Vname 
  | Plus AExp AExp 
  deriving (Show)

type Val   = Int 
type State = S.GState Vname Val 

{-@ reflect aval @-}
aval                :: AExp -> State -> Val 
aval (N n) _        = n 
aval (V x) s        = S.get s x 
aval (Plus e1 e2) s = aval e1 s + aval e2 s


{- reflect zero @-}
zero :: State 
zero = S.init  0

exp0 :: AExp 
exp0 = Plus (N 3) (V "x")

value0 :: Val
value0 = aval exp0 zero 

{-@ reflect asimp_const @-}
{-@ asimp_const :: _ -> {v:_ | optimal v} @-} 
asimp_const :: AExp -> AExp 
asimp_const (N n) = N n
asimp_const (V x) = V x  
asimp_const (Plus a1 a2) = case (asimp_const a1, asimp_const a2) of 
  (N n1, N n2) -> N (n1 + n2) 
  (b1  , b2)   -> Plus b1 b2

{-@ ple lemma_aval_asimp_const @-}
{-@ lemma_aval_asimp_const :: a:_ -> s:_ -> 
      { aval (asimp_const a) s = aval a s } @-}
lemma_aval_asimp_const :: AExp -> State -> Proof
lemma_aval_asimp_const (N _) _ 
  = ()
lemma_aval_asimp_const (V _) _ 
  = ()
lemma_aval_asimp_const (Plus a1 a2) s 
  = case (asimp_const a1, asimp_const a2) of 
      (N _, N _) -> lemma_aval_asimp_const a1 s &&& lemma_aval_asimp_const a2 s 
      (b1  , b2) -> lemma_aval_asimp_const a1 s &&& lemma_aval_asimp_const a2 s 

{-@ reflect plus @-}
plus :: AExp -> AExp -> AExp 
plus (N i1) (N i2) = N (i1+i2) 
plus (N 0)  a      = a 
plus a      (N 0)  = a 
plus a1     a2     = Plus a1 a2

{-@ ple lemma_aval_plus @-}
{-@ lemma_aval_plus :: a1:_ -> a2:_ -> s:_ -> 
    { aval (plus a1 a2) s = aval a1 s + aval a2 s } @-}
lemma_aval_plus :: AExp -> AExp -> State -> Proof 
lemma_aval_plus (N _) (N _) _ = () 
lemma_aval_plus (N 0) a     _ = () 
lemma_aval_plus a     (N 0) _ = () 
lemma_aval_plus a1    a2    _ = () 

{-@ reflect asimp @-}
asimp :: AExp -> AExp 
asimp (Plus a1 a2) = plus (asimp a1) (asimp a2)
asimp a            = a 

{-@ ple lemma_aval_asimp @-}
{-@ lemma_aval_asimp :: a:_ -> s:_ -> 
      { aval (asimp a) s = aval a s } @-}
lemma_aval_asimp :: AExp -> State -> Proof 
lemma_aval_asimp (Plus a1 a2) s = lemma_aval_asimp a1 s 
                              &&& lemma_aval_asimp a2 s 
                              &&& lemma_aval_plus (asimp a1) (asimp a2) s
lemma_aval_asimp a            s =  () 

--------------------------------------------------------------------------------
-- | Exercise 3.1 
--------------------------------------------------------------------------------

{-@ measure optimal @-}
optimal :: AExp -> Bool 
optimal (N _) = True 
optimal (V _) = True 
optimal (Plus a1 a2) = optimal a1 && optimal a2 && not (const a1 && const a2)

{-@ measure const @-}
const :: AExp -> Bool 
const (N _) = True 
const _     = False 

-- We prove `optimal (asimp_const a)` by typing `asimp_const :: AExp -> {v:AExp | optimal v}` 

--------------------------------------------------------------------------------
-- | Exercise 3.2 
--------------------------------------------------------------------------------

{-@ measure is_normal @-}
is_normal :: AExp -> Bool 
is_normal (Plus a1 a2) = is_var a1 && is_normal a2 
is_normal a            = True 

{-@ measure is_var @-}
is_var :: AExp -> Bool 
is_var (V _) = True 
is_var _     = False 

{-@ type NAExp = {a:AExp | is_normal a} @-}

{-@ reflect cplus @-}
{-@ cplus :: Int -> NAExp -> NAExp @-}
cplus :: Int -> AExp -> AExp
cplus n (Plus a1 a2) = Plus a1    (cplus n a2)
cplus n (V x)        = Plus (V x) (N n) 
cplus n (N m)        = N (n + m)

{-@ ple lemma_cplus @-}
{-@ lemma_cplus :: n:Int -> a:NAExp -> s:_ -> 
      { aval (cplus n a) s = n + aval a s } 
  @-}
lemma_cplus :: Int -> AExp -> State -> Proof 
lemma_cplus n (Plus a1 a2) s = lemma_cplus n a2 s 
lemma_cplus n (V _)        _ = () 
lemma_cplus n (N _)        _ = () 

{-@ reflect nplus @-}
{-@ nplus :: NAExp -> NAExp -> NAExp @-} 
nplus :: AExp -> AExp -> AExp 
nplus (Plus a1 a2) b = Plus a1    (nplus a2 b)
nplus (V x)        b = Plus (V x) b 
nplus (N n)        b = cplus n b

{-@ ple lemma_nplus @-}
{-@ lemma_nplus :: a:NAExp -> b:NAExp -> s:_ -> 
      { aval (nplus a b) s = aval a s + aval b s } 
  @-}
lemma_nplus :: AExp -> AExp -> State -> Proof 
lemma_nplus (Plus a1 a2) b s = lemma_nplus a2 b s
lemma_nplus (V x)        b s = () 
lemma_nplus (N n)        b s = lemma_cplus n b s 

{-@ reflect full_asimp @-}
{-@ full_asimp :: AExp -> NAExp @-} 
full_asimp :: AExp -> AExp 
full_asimp (Plus a1 a2) = nplus (full_asimp a1) (full_asimp a2)
full_asimp a            = a 

{-@ ple lemma_full_asimp @-}
{-@ lemma_full_asimp :: a:_ -> s:_ -> 
      { aval (full_asimp a) s = aval a s } @-}
lemma_full_asimp :: AExp -> State -> Proof 
lemma_full_asimp (Plus a1 a2) s 
  =   lemma_full_asimp a1 s
  &&& lemma_full_asimp a2 s
  &&& lemma_nplus (full_asimp a1) (full_asimp a2) s
lemma_full_asimp _ _ 
  =   ()



--------------------------------------------------------------------------------
-- | Exercise 3.3 
--------------------------------------------------------------------------------

{-@ reflect asgn @-}
asgn :: Vname -> AExp -> State -> State
asgn x a s = S.set s x (aval a s)

{-@ reflect subst @-}
subst :: Vname -> AExp -> AExp -> AExp 
subst x e (Plus a1 a2)   = Plus (subst x e a1) (subst x e a2) 
subst x e (V y) | x == y = e 
subst _ _ a              = a 

{-@ ple lemma_subst @-}
{-@ lemma_subst :: x:_ -> a:_ -> e:_ -> s:_ -> { aval (subst x a e) s = aval e (asgn x a s) } @-}
lemma_subst :: Vname -> AExp -> AExp -> State -> Proof 
lemma_subst x a (V y) s   
  | x == y                     = () 
  | otherwise                  = S.lemma_get_not_set y x (aval a s) s 
lemma_subst x a (N i) s        = ()
lemma_subst x a (Plus e1 e2) s = lemma_subst x a e1 s &&& lemma_subst x a e2 s

{- 
lemma_subst x a (V y) s 
  | x == y
  =   let s' = asgn x a s in -- upd s x (aval a s) in  
      aval (subst x a (V y)) s 
  === aval a s
  === S.get (S.set s x (aval a s)) y
  === aval (V y) s' 
  *** QED 

  | otherwise 
  =   let 
           s' = asgn x a s 
      in 
      aval (subst x a (V y)) s 
  === aval (V y) s
  === S.get s  y  
      ? S.lemma_get_not_set y x (aval a s) s 
  === S.get s' y
  -- === S.get (S.set s x (aval a s)) y
  === aval (V y) s' 
  *** QED 

lemma_subst x a (N i) s
  = () 
  -- undefined 
  -- =   let s' = upd s x (aval a s) in  
      -- aval (subst x a (N i)) s 
  -- === i
  -- === aval (N i) s' 
  -- *** QED 

lemma_subst x a (Plus e1 e2) s
  = 
    (lemma_subst x a e1 s &&& lemma_subst x a e2 s) 
  -- =   let s' = upd s x (aval a s) in  
      -- aval (subst x a (Plus e1 e2)) s 
  -- === aval (Plus (subst x a e1) (subst x a e2)) s
  -- === aval (subst x a e1) s + aval (subst x a e2) s
    -- ? (lemma_subst x a e1 s &&& lemma_subst x a e2 s) 
  -- === aval e1 s'            + aval e2 s'
  -- === aval (Plus e1 e2) s' 
  -- *** QED 
-}

{-@ ple lemma_subst_eq @-}
{-@ lemma_subst_eq :: x:_ -> a1:_ -> a2:_ -> e:_ -> s:{aval a1 s = aval a2 s} -> 
      { aval (subst x a1 e) s = aval (subst x a2 e) s} 
  @-}
lemma_subst_eq :: Vname -> AExp -> AExp -> AExp -> State -> Proof 
lemma_subst_eq x a1 a2 e s = lemma_subst x a1 e s &&& lemma_subst x a2 e s 

{- 
lemma_subst_eq x a1 a2 e s 
   =   aval (subst x a1 e) s 
       ? lemma_subst x a1 e s  
   === aval e (upd s x (aval a1 s))
   === aval e (upd s x (aval a2 s))
       ? lemma_subst x a2 e s  
   === aval (subst x a2 e) s 
   *** QED 
-}

--------------------------------------------------------------------------------
-- | Exercise: 3.6 
--------------------------------------------------------------------------------

data LExp 
  = LN    Int 
  | LV    Vname 
  | LPlus LExp  LExp 
  | LLet  Vname LExp LExp  
  deriving (Show)

{-@ reflect lval @-}
lval :: LExp -> State -> Int 
lval (LN i) _         = i 
lval (LV x) s         = S.get s x 
lval (LPlus e1 e2)  s = lval e1 s + lval e2 s 
lval (LLet x e1 e2) s = lval e2 (S.set s x (lval e1 s)) 

{-@ reflect inlyne @-}
inlyne :: LExp -> AExp 
inlyne (LN i)         = N i 
inlyne (LV x)         = V x 
inlyne (LPlus  e1 e2) = Plus (inlyne e1) (inlyne e2)
inlyne (LLet x e1 e2) = subst x (inlyne e1) (inlyne e2) 

{-@ ple lemma_inlyne @-}
{-@ lemma_inlyne :: l:_ -> s:_ -> { lval l s = aval (inlyne l) s } @-} 
lemma_inlyne :: LExp -> State -> Proof 
lemma_inlyne (LN i)         s = ()
lemma_inlyne (LV x)         s = ()
lemma_inlyne (LPlus  e1 e2) s = lemma_inlyne e1 s &&& lemma_inlyne e2 s
lemma_inlyne (LLet x e1 e2) s = lemma_inlyne e1 s &&& lemma_inlyne e2 s' &&& lemma_subst x a1 a2 s 
  where 
    a1                        = inlyne e1 
    a2                        = inlyne e2 
    s'                        = S.set s x (lval e1 s)
    
--------------------------------------------------------------------------------
-- | Section: 3.2 
--------------------------------------------------------------------------------

-- See NNF etc.

data BExp 
  = Bc   Bool 
  | Not  BExp 
  | And  BExp BExp 
  | Less AExp AExp 
  deriving (Show)

{-@ reflect bOr @-}
bOr :: BExp -> BExp -> BExp 
bOr b1 b2 = Not ((Not b1) `And` (Not b2))
       
{-@ reflect bImp @-}
bImp :: BExp -> BExp -> BExp 
bImp b1 b2 = bOr (Not b1) b2

{-@ reflect bval @-}
bval :: BExp -> State -> Bool
bval (Bc   b)     s = b 
bval (Not  b)     s = not (bval b s) 
bval (And  b1 b2) s = bval b1 s && bval b2 s 
bval (Less a1 a2) s = aval a1 s <  aval a2 s 

{-@ reflect bNot @-}
bNot :: BExp -> BExp 
bNot (Bc True)  = Bc False 
bNot (Bc False) = Bc True 
bNot b          = Not b

{-@ reflect bAnd @-}
bAnd :: BExp -> BExp -> BExp 
bAnd (Bc True)  b          = b
bAnd b          (Bc True)  = b
bAnd (Bc False) b          = Bc False
bAnd b          (Bc False) = Bc False
bAnd b1         b2         = And b1 b2

{-@ reflect bLess @-}
bLess :: AExp -> AExp -> BExp 
bLess (N n1) (N n2) = Bc (n1 < n2) 
bLess a1     a2     = Less a1 a2

{-@ reflect bsimp @-}
bsimp :: BExp -> BExp
bsimp (Bc v)       = Bc v 
bsimp (Not b)      = bNot  (bsimp b)
bsimp (And b1 b2)  = bAnd  (bsimp b1) (bsimp b2)
bsimp (Less a1 a2) = bLess (asimp a1) (asimp a2)


--------------------------------------------------------------------------------
-- | Section 3.3 
--------------------------------------------------------------------------------

data Instr 
  = LOADI Val 
  | LOAD  Vname 
  | ADD
  deriving (Show)

type Stack = [Val]

{-@ reflect exec1 @-}
exec1 :: Instr -> State -> Stack -> Stack
exec1 (LOADI n) _ stk       = n     : stk 
exec1 (LOAD x)  s stk       = (S.get s x) : stk
exec1 ADD       _ (j:i:stk) = (i+j) : stk 
exec1 _         _ stk       = [] 

{-@ reflect exec @-}
exec :: [Instr] -> State -> Stack -> Stack 
exec []     _ stk = stk 
exec (i:is) s stk = exec is s (exec1 i s stk)

{-@ reflect comp @-}
comp :: AExp -> [Instr]
comp (N n)        = [LOADI n]
comp (V x)        = [LOAD x]
comp (Plus a1 a2) = comp a1 ++ (comp a2 ++ [ADD])

{-@ ple lemma_comp @-}
{-@ lemma_comp :: a:_ -> s:_ -> stk:_ -> 
      { exec (comp a) s stk = cons (aval a s) stk } @-}
lemma_comp :: AExp -> State -> Stack -> Proof 
lemma_comp (N n)        s stk = () 
lemma_comp (V x)        s stk = () 
lemma_comp (Plus a1 a2) s stk = lemma_exec_append (comp a1) (comp a2 ++ [ADD]) s stk
                            &&& lemma_exec_append (comp a2) [ADD] s stk1
                            &&& lemma_comp a1 s stk 
                            &&& lemma_comp a2 s stk1
  where 
    stk2                      = exec (comp a2) s stk1
    stk1                      = exec (comp a1) s stk

{-@ ple lemma_exec_append @-}
{-@ lemma_exec_append :: is1:_ -> is2:_ -> s:_ -> stk:_ -> 
      { exec (is1 ++ is2) s stk = exec is2 s (exec is1 s stk) }
  @-}
lemma_exec_append :: [Instr] -> [Instr] -> State -> Stack -> Proof
lemma_exec_append []       is2 s stk = () 
lemma_exec_append (i1:is1) is2 s stk = lemma_exec_append is1 is2 s (exec1 i1 s stk)

--------------------------------------------------------------------------------
-- | Register State [Required by Ex 3.11 and 3.12]
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- | Exercise 3.11
--------------------------------------------------------------------------------

type Reg    = Int 

-- NOTE: We use this strange `RegState` indirection 
-- instead of the raw function, because when you reflect 
-- a function with the output `RegState` it ends up putting 
-- the reflection in the WRONG PLACE. 

type RegState = Reg -> Int

{-@ reflect rset @-}
rset :: (Reg -> Int) -> Reg -> Val -> (Reg -> Int)
rset f r v reg = if reg == r then v else f reg 

{-@ ple lemma_get_set @-}
{-@ lemma_get_set :: r:_ -> v:_ -> rs:_ -> { rset rs r v r = v }  @-}
lemma_get_set :: Reg -> Val -> RegState -> Proof 
lemma_get_set _ _ _ = () 

{-@ ple lemma_get_not_set @-}
{-@ lemma_get_not_set :: r0:_ -> r:{r /= r0} -> val:_ -> rs:_ -> { rset rs r val r0 = rs r0 }  @-}
lemma_get_not_set :: Reg -> Reg -> Val -> RegState -> Proof 
lemma_get_not_set _ _ _ _ = () 

data RInstr 
  = RLOADI Reg Val    -- ^ moves `Int`   into `Reg` 
  | RLOAD  Reg Vname  -- ^ moves `Vname` into `Reg` 
  | RADD   Reg Reg    -- ^ increments `Reg` by `Reg`
  deriving (Show)

{-@ reflect rexec1 @-}
rexec1 :: RInstr -> State -> RegState -> RegState 
rexec1 (RLOADI r i)   s rs z = rset rs r  i     z 
rexec1 (RLOAD  r x)   s rs z = rset rs r  (S.get s x) z
rexec1 (RADD   r1 r2) s rs z = rset rs r1 (rs r1 + rs r2) z 

{-@ reflect rexec @-}
rexec :: [RInstr] -> State -> RegState -> RegState 
rexec []     _ rs z = rs z 
rexec (i:is) s rs z = rexec is s (rexec1 i s rs) z 

{-@ reflect rcomp @-}
rcomp :: AExp -> Reg -> [RInstr]
rcomp (N n)        r = [RLOADI r n]
rcomp (V x)        r = [RLOAD  r x]
rcomp (Plus a1 a2) r = rcomp a1 r ++ (rcomp a2 (r+1) ++ [RADD r (r+1)])

{-@ ple lemma_rcomp @-}
{-@ lemma_rcomp :: a:_ -> r:_ -> s:_ -> rs:_ -> 
      { rexec (rcomp a r) s rs r = aval a s } 
  @-}
lemma_rcomp :: AExp -> Reg -> State -> RegState -> Proof 
lemma_rcomp (N n)        r s rs 
  = () 
lemma_rcomp (V x)        r s rs 
  = () 
lemma_rcomp (Plus a1 a2) r s rs 
  = () 
  ? lemma_rexec_append is1 (is2 ++ is3) s rs  r 
  ? lemma_rexec_append is2 is3          s rs1 r
  ? lemma_stomp a2 r (r+1) s rs1
  ? lemma_rcomp a1 r s rs  
  ? lemma_rcomp a2 (r+1)   s rs1
  where 
    is1 = rcomp a1 r 
    is2 = rcomp a2 (r+1)
    is3 = [RADD r (r+1)]
    rs1 = rexec is1 s rs
    rs2 = rexec is2 s rs1
    rs3 = rset rs2  r val 
    val = rs2 r + rs2 (r+1)

{-@ ple lemma_stomp @-}
{-@ lemma_stomp :: a:_ -> r0:_ -> r:{r0 < r} -> s:_ -> rs:_ -> 
     { rexec (rcomp a r) s rs r0 = rs r0 } @-}
lemma_stomp :: AExp -> Reg -> Reg -> State -> RegState -> Proof 
lemma_stomp (N _)        r0 r s rs 
  = ()
lemma_stomp (V _)        r0 r s rs 
  = ()
lemma_stomp (Plus a1 a2) r0 r s rs 
  = () 
  ? lemma_rexec_append is1 (is2 ++ is3) s rs  r0  
  ? lemma_rexec_append is2 is3          s rs1 r0
  ? lemma_stomp a2 r0 (r+1) s rs1 
  ? lemma_stomp a1 r0 r     s rs
  where
    is1 = rcomp a1 r 
    is2 = rcomp a2 (r+1)
    is3 = [RADD r (r+1)]
    rs1 = rexec is1 s rs
    rs2 = rexec is2 s rs1
    val = rs2 r + rs2 (r+1)

-- TODO: This is a silly consequence of the eta-expansion above. Grr. 

{-@ ple lemma_rexec_append @-}
{-@ lemma_rexec_append :: is1:_ -> is2:_ -> s:_ -> rs:_ -> r0:_ ->  
      { rexec (is1 ++ is2) s rs r0 = rexec is2 s (rexec is1 s rs) r0 } @-}
lemma_rexec_append :: [RInstr] -> [RInstr] -> State -> RegState -> Reg -> Proof 
lemma_rexec_append []       is2 s rs r0 
  = () 
  ? axiom_fun_eq (rexec [] s rs) rs (\_ -> ()) 

lemma_rexec_append (i1:is1) is2 s rs r0 
  = () 
  ? lemma_rexec_append is1 is2 s rs1  r0 
  ? axiom_fun_eq (rexec is1 s rs1) (rexec (i1:is1) s rs) (\_ -> ())
  where 
    rs1 = rexec1 i1 s rs 

--------------------------------------------------------------------------------
-- | Exercise 3.12 (see Ch03_ex_12.hs)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-@ assume axiom_fun_eq :: forall a b <pa :: a -> Bool, pb :: b -> Bool>. 
        f:(a<pa> -> b<pb>) 
     -> g:(a<pa> -> b<pb>) 
     -> (x:a<pa> -> { f x = g x }) 
     -> {f = g}  
  @-}

{- assume axiom_fun_eq :: f:_ -> g:_ -> (x:_ -> { f x = g x}) -> { f = g } @-}
axiom_fun_eq :: (a -> b) -> (a -> b) -> (a -> Proof) -> Proof 
axiom_fun_eq _ _ _ = () 

{-@ reflect adder @-}
adder :: Int -> Int -> Int -> Int 
adder x y z = x + y + z 

{-@ ple lemma_adder @-}
{-@ lemma_adder :: () -> { adder 1 2  = adder 2 1 } @-}
lemma_adder :: () -> ()
lemma_adder _ = axiom_fun_eq (adder 1 2) (adder 2 1) (\_ -> ())

\end{code}