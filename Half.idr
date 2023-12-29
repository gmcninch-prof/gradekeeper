data Parity : Nat -> Type where
   Even : {n : _} -> Parity (n + n)
   Odd  : {n : _} -> Parity (S (n + n))

parity : (n : Nat) -> Parity n
parity Z = Even {n = Z}
parity (S Z) = Odd {n = Z}
parity (S (S k)) with (parity k)
  parity (S (S (j + j))) | Even
      = rewrite plusSuccRightSucc j j in Even {n = S j}
  parity (S (S (S (j + j)))) | Odd
      = rewrite plusSuccRightSucc j j in Odd {n = S j}

divTwo : Nat -> Nat
divTwo k with (parity k)
  divTwo (n + n) | Even = n
  divTwo (S (n + n)) | Odd = (S n)

  
      
halfIneqS : { k: Nat} -> {0 _ : IsSucc k} -> LTE (S k) (2*k) -> LTE (S (S k)) (2*(S k))    
halfIneqS {k = 0} pf impossible
halfIneqS {k = (S k)} pf = 
  ?f $ LTESucc pf


LTEreflexive : {k : Nat} -> LTE k k
LTEreflexive {k = 0} = LTEZero
LTEreflexive {k = (S k)} = LTESucc $ LTEreflexive {k}

sumIneq : { k, l : Nat } -> {0 _ : IsSucc l} -> LTE (S k) (k + l)
sumIneq {k = 0} = ?LTESucc LTEZero
sumIneq {k = (S k)} {l = 0} = ?LTEreflexive_asdfsdf
sumIneq {k = (S k)} {l = (S j)} = ?sumIneq_rhs_7


halfIneq : {k : Nat} -> {0 _ : IsSucc k} -> LTE (S k) (k+k)
halfIneq {k = 0} impossible
halfIneq {k = 1} = LTEreflexive 
halfIneq {k = (S k)} = 
  ?lteSuccLeft $ plusLteMonotoneLeft (S k) 0 (S k) LTEZero 



divTwoF : (n:Nat) -> {0 _ : IsSucc n} -> Fin n  
divTwoF n with (parity n)
  divTwoF (k + k) | Even = natToFinLT k {prf}
    where
      prf : LTE (S k) (k+k)
      prf = case k of
                 0 impossible
                 (S j) => ?prf_rhs_1
  divTwoF (S (k+k)) | Odd = natToFinLT k {prf}
    where
      prf : LTE (S k) (S (k + k))
      prf = case k of
                 0 => ?prf_rhs_0
                 (S j) => ?prf_rhs_2

