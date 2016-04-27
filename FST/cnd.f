C**********************************************************************
      FUNCTION INIT(NREG,X)
      IMPLICIT NONE
      REAL*8 PI
      PARAMETER ( PI=3.141592653589793D0 )
      INTEGER NREG
      REAL*8 INIT,X
      REAL*8 FFF,Q,RL,RR,RHO,VEL
      REAL*8 PAR(9)
      COMMON/USER_PARAM/ PAR
      REAL*8 BETA,TAU,XMAX
      COMMON/REAL_PARAM/ BETA,TAU,XMAX
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c      FFF(X)=(TANH(4.*(2.*X-1.))+1.)/2.
      FFF(X)=X
      Q =PAR(1) ! Entry Q
      RL=PAR(2) ! Left Rho
      RR=PAR(3) ! Right Rho
      RHO=(RR-RL)*FFF(X/XMAX)+RL
      VEL=Q/RHO
      IF(NREG.EQ.1)THEN      !!! dencity
       INIT=RHO
      ELSE IF(NREG.EQ.2)THEN !!! velosity
       INIT=VEL
      ELSE
       WRITE(0,*) ' "INIT": NREG can''t be ',NREG
       STOP
      END IF
      RETURN
      END

C**********************************************************************
      FUNCTION BC(NREG,U,V,UX,VX,UT,VT,X,T)
      IMPLICIT NONE
      REAL*8 PI,DLT
      PARAMETER ( PI=3.141592653589793D0,DLT=1.D-6 )
      INTEGER NREG,NFR
      REAL*8 BC,U,V,X,T,UX,VX,UT,VT
      REAL*8 R1T,R1X,R2T,R2X,LD1,LD2,A,V0
      REAL*8 Q,RL,RR
      REAL*8 PAR(9)
      COMMON/USER_PARAM/ PAR
      REAL*8 BETA,TAU,XMAX
      COMMON/REAL_PARAM/ BETA,TAU,XMAX
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      Q =PAR(1) ! Entry Q
      RL=PAR(2) ! Left Rho
      RR=PAR(3) ! Right Rho
c
      NFR=NINT(PAR(4)) ! Use free boundaries?
c
      R1T=VT-BETA/U*A(U,X,T)*UT
      R1X=VX-BETA/U*A(U,X,T)*UX
      R2T=VT+1.D0/U*A(U,X,T)*UT
      R2X=VX+1.D0/U*A(U,X,T)*UX
      LD1=V-1.D0*A(U,X,T) ! Slow Characteristics
      LD2=V+BETA*A(U,X,T) ! Fast Characteristics
c
      IF(X.EQ.0.D0)THEN
       IF(NREG.EQ.1)BC=U-RL
       IF(NREG.EQ.1.AND.LD1.LT.0.D0.AND.NFR.NE.0)! Free boundary
     *         BC=R1T+LD1*R1X-(V0(U,X,T)-V)/TAU
       IF(NREG.EQ.2)BC=U*V-Q
       IF(NREG.EQ.2.AND.LD2.LT.0.D0.AND.NFR.NE.0)! Free boundary
     *         BC=R2T+LD2*R2X-(V0(U,X,T)-V)/TAU
      ELSE
       IF(NREG.EQ.1)BC=U-RR
       IF(NREG.EQ.1.AND.LD2.GT.0.D0.AND.NFR.NE.0)! Free boundary
     *         BC=R2T+LD2*R2X-(V0(U,X,T)-V)/TAU
       IF(NREG.EQ.2)BC=U*V-Q
       IF(NREG.EQ.2.AND.LD1.GT.0.D0.AND.NFR.NE.0)! Free boundary
     *         BC=R1T+LD1*R1X-(V0(U,X,T)-V)/TAU
      END IF
      RETURN
      END


C**********************************************************************
      FUNCTION FLX(X,T)
      IMPLICIT NONE
      REAL*8 FLX,X,T
      REAL*8 PAR(9)
      COMMON/USER_PARAM/ PAR
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      FLX=0.D0
      RETURN
      END

C**********************************************************************
      FUNCTION FRC(X,T)
      IMPLICIT NONE
      REAL*8 FRC,X,T
      REAL*8 PAR(9)
      COMMON/USER_PARAM/ PAR
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      FRC=0.D0
      RETURN
      END

