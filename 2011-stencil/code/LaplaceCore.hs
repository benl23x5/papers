
-- Core of Laplace benchmark for the SolverGet version
-- NOTE: shifted the world parameter to the start for formatting.
case quotInt# ixLinear width of { iX ->
case remInt#  ixLinear width of { iY -> 
 writeDoubleArray# world arrDest ixLinear
  (+##
   (indexDoubleArray# arrBV
    (+# arrBV_start (+# (*# iY arrBV_width) iX)))
   (*##
    (indexDoubleArray# arrBM 
     (+# arrBM_start (+# (*# iY arrBM_width) iX)))
    (/##
     (+##
      (+##
       (+##
        (indexDoubleArray# arrSrc
         (+# arrSrc_start (+# (*# (-# width 1) iY) iX)))
        (indexDoubleArray# arrSrc
         (+# arrSrc_start (+# (*# width iY) (-# iX 1)))))
       (indexDoubleArray# arrSrc
        (+# arrSrc_start (+# (*# (+# width 1) iY) iX))))
      (indexDoubleArray# arrSrc
       (+# arrSrc_start (+# (*# width iY) (+# iX 1)))))
     4.0)))
 }}




-- Core of Laplace benchmark for the SolverStencil version
case quotInt# ixLinear width  of { iX ->
case remInt#  ixLinear width  of { iY -> 
case +# iX (*# iY width) of { ixCenter ->
 writeDoubleArray# world arrDest ixLinear
  (+##
   (indexDoubleArray# arrBV 
    (+# arrBV_start (+# (*# iY arrBV_width) iX)))
   (*##
    (indexDoubleArray# arrBM 
     (+# arrBM_mask (+# (*# iY arrBM_width) iX)))
    (/##
     (+##
      (+##
       (+##
        (indexDoubleArray# arrSrc 
         (+# arrSrc_start (+# ixCenter width)))
        (indexDoubleArray# arrSrc
         (+# arrSrc_start (+# ixCenter 1))))
       (indexDoubleArray# arrSrc
        (+# arrSrc_start (+# ixCenter (-1)))))
      (indexDoubleArray# arrSrc
       (+# arrSrc_start (+# ixCenter (*# (-1) width)))))
     4.0)))
 }}


case quotInt# ixLinear width  of { iX ->
case remInt#  ixLinear width  of { iY -> 
case +# iX (*# iY width) of { ixCenter ->
 writeDoubleArray# world arrDest ixLinear
  (+##
   (indexDoubleArray# arrBV 
    (+# arrBV_start (+# (*# iY arrBV_width) iX)))
   (*##
    (indexDoubleArray# arrBM 
     (+# arrBM_mask (+# (*# iY arrBM_width) iX)))
    (/##
     (+##
      (+##
       (+##
        let startCenter = +# arrSrc_start ixCenter
        (indexDoubleArray# arrSrc 
         (+# startCenter width))
        (indexDoubleArray# arrSrc
         (+# startCenter 1))))
       (indexDoubleArray# arrSrc
        (+# startCenter (-1)))))
      (indexDoubleArray# arrSrc
       (+# startCenter (*# (-1) width)))))
     4.0)))
 }}
