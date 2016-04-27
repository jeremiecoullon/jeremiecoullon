C**********************************************************************
      FUNCTION FLUX(U,X,T)
      IMPLICIT NONE
      REAL*8 PI
      PARAMETER ( PI=3.141592653589793D0 )
      REAL*8 FLUX,U,X,T,UMAX
      REAL*8 PAR(9)
      COMMON/PARAM/ PAR
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      FLUX=U*EXP(-(2*U)**3)
c      FLUX=U*EXP(-(3*U)**2)
c      FLUX=SIN(PI*U)/PI
      RETURN
      END
