      IMPLICIT NONE

      INTEGER NNRHO
      PARAMETER( NNRHO=1500 )

      CHARACTER*25 FILEOUT,FILE

      INTEGER LENGTHOUT,NRHO,NPAR,I,K,L
      REAL*8 RHOMIN,X,T,DELTARHO
      REAL*8 P,A,V0,H
      REAL*8 RHO(NNRHO)

      REAL*8 PAR(9)
      COMMON/PAR/ PAR
      REAL*8 RHOMAX
      COMMON/RHOMAX/ RHOMAX
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      X=0.D0
      T=0.D0

      READ*,FILE

      print*,' '
      print*,' Reading initialization file ',FILE
      print*,' '
C***********
      OPEN  (1,FILE=FILE,STATUS='OLD',FORM='FORMATTED',ERR=620)
      READ  (1,*) FILEOUT
      READ  (1,*) NRHO
      READ  (1,*) NPAR
       DO I=1,NPAR
        READ  (1,*) PAR(I)
       END DO
      READ  (1,*) RHOMAX
      CLOSE (1)
C
      WRITE(*,'('' FILEOUT = '',A25)'  ) FILEOUT
      WRITE(*,'('' NRHO    = '',I6)'   ) NRHO
      WRITE(*,'('' NPAR    = '',I6)'   ) NPAR
       DO I=1,NPAR
        WRITE(*,'(''  PAR(''I1'')  = '',D13.7)') I,PAR(I)
       END DO
      WRITE(*,'('' RHOMAX  = '',D13.7)') RHOMAX
C**********

      NRHO=(NRHO/4)*4

      IF(NRHO.GT.NNRHO)THEN
       PRINT*,' "NRHO" should be less than ',NNRHO
       STOP
      END IF

      DO I=1,25
       IF(FILEOUT(I:I).EQ.' '.OR.FILEOUT(I:I+1).EQ.'.f')THEN
        LENGTHOUT=I-1
        GO TO 9192
       END IF
      END DO
 9192 CONTINUE

      RHOMIN=0.
      DO L=1,NRHO
       RHO(L)=RHOMIN+(RHOMAX-RHOMIN)*(L-1.D0)/(NRHO-1.D0)
      END DO
      DELTARHO=RHO(2)-RHO(1)

      PRINT*,' '
      PRINT*,' Start '
      PRINT*,' '
      FILE=FILEOUT(1:LENGTHOUT)//'.f'
      OPEN(11,FILE=FILE,FORM='FORMATTED',STATUS='UNKNOWN',ERR=620)
      REWIND(11)

      WRITE(11,'(''C***********************************************'')')
      WRITE(11,'(5X'' FUNCTION P(RHO,X,T)'')')
      WRITE(11,'(5X'' IMPLICIT NONE'')')
      WRITE(11,'(5X'' INTEGER NN'')')
      WRITE(11,'(5X'' PARAMETER ( NN='',I4,'' )'')') NRHO
      WRITE(11,'(5X'' REAL*8 RHOMAX'')')
      WRITE(11,'(5X'' PARAMETER ( RHOMAX='',E13.7,'' )'')') RHOMAX
      WRITE(11,'(5X'' REAL*8 DELTARHO'')')
      WRITE(11,'(5X'' PARAMETER ( DELTARHO='',E13.7,'' )'')') DELTARHO
      WRITE(11,'(5X'' INTEGER I'')')
      WRITE(11,'(5X'' REAL*8 P,RHO,X,T,RHOI'')')
      WRITE(11,'(5X'' REAL PP(NN)'')')
      WRITE(11,'(5X'' DATA PP'')')
      WRITE(11,'(5X''* /'')')
      DO K=1,NRHO/4-1
       WRITE(11,104) (P(RHO(L+4*(K-1)),X,T),L=1,4)
      END DO
      K=NRHO/4
      WRITE(11,105) (P(RHO(L+4*(K-1)),X,T),L=1,4)
      WRITE(11,'(5X''* /'')')
      WRITE(11,'(5X'' IF(RHO.LE.0.D0)THEN'')')
      WRITE(11,'(5X''  P=PP(1)'')')
      WRITE(11,'(5X'' ELSE IF(RHO.GE.RHOMAX)THEN'')')
      WRITE(11,'(5X''  P=PP(NN)'')')
      WRITE(11,'(5X'' ELSE'')')
      WRITE(11,'(5X''  I=INT(RHO/DELTARHO)+1'')')
      WRITE(11,'(5X''  RHOI=RHOMAX*(I-1.D0)/(NN-1.D0)'')')
      WRITE(11,'(5X''  P=PP(I)+(RHO-RHOI)*(PP(I+1)-PP(I))/DELTARHO'')')
      WRITE(11,'(5X'' END IF'')')
      WRITE(11,'(5X'' RETURN'')')
      WRITE(11,'(5X'' END'')')

      WRITE(11,'('''')')
      WRITE(11,'(''C***********************************************'')')
      WRITE(11,'(5X'' FUNCTION H(RHO,X,T)'')')
      WRITE(11,'(5X'' IMPLICIT NONE'')')
      WRITE(11,'(5X'' INTEGER NN'')')
      WRITE(11,'(5X'' PARAMETER ( NN='',I4,'' )'')') NRHO
      WRITE(11,'(5X'' REAL*8 RHOMAX'')')
      WRITE(11,'(5X'' PARAMETER ( RHOMAX='',E13.7,'' )'')') RHOMAX
      WRITE(11,'(5X'' REAL*8 DELTARHO'')')
      WRITE(11,'(5X'' PARAMETER ( DELTARHO='',E13.7,'' )'')') DELTARHO
      WRITE(11,'(5X'' INTEGER I'')')
      WRITE(11,'(5X'' REAL*8 P,H,RHO,X,T,RHOI'')')
      WRITE(11,'(5X'' REAL PP(NN)'')')
      WRITE(11,'(5X'' DATA PP'')')
      WRITE(11,'(5X''* /'')')
      DO K=1,NRHO/4-1
       WRITE(11,104) (H(RHO(L+4*(K-1)),X,T),L=1,4)
      END DO
      K=NRHO/4
      WRITE(11,105) (H(RHO(L+4*(K-1)),X,T),L=1,4)
      WRITE(11,'(5X''* /'')')
      WRITE(11,'(5X'' IF(RHO.LE.0.D0)THEN'')')
      WRITE(11,'(5X''  P=PP(1)'')')
      WRITE(11,'(5X'' ELSE IF(RHO.GE.RHOMAX)THEN'')')
      WRITE(11,'(5X''  P=PP(NN)'')')
      WRITE(11,'(5X'' ELSE'')')
      WRITE(11,'(5X''  I=INT(RHO/DELTARHO)+1'')')
      WRITE(11,'(5X''  RHOI=RHOMAX*(I-1.D0)/(NN-1.D0)'')')
      WRITE(11,'(5X''  P=PP(I)+(RHO-RHOI)*(PP(I+1)-PP(I))/DELTARHO'')')
      WRITE(11,'(5X'' END IF'')')
      WRITE(11,'(5X'' H=P'')')
      WRITE(11,'(5X'' RETURN'')')
      WRITE(11,'(5X'' END'')')

      WRITE(11,'('''')')
      WRITE(11,'(''C***********************************************'')')
      WRITE(11,'(5X'' FUNCTION A(RHO,X,T)'')')
      WRITE(11,'(5X'' IMPLICIT NONE'')')
      WRITE(11,'(5X'' INTEGER NN'')')
      WRITE(11,'(5X'' PARAMETER ( NN='',I4,'' )'')') NRHO
      WRITE(11,'(5X'' REAL*8 RHOMAX'')')
      WRITE(11,'(5X'' PARAMETER ( RHOMAX='',E13.7,'' )'')') RHOMAX
      WRITE(11,'(5X'' REAL*8 DELTARHO'')')
      WRITE(11,'(5X'' PARAMETER ( DELTARHO='',E13.7,'' )'')') DELTARHO
      WRITE(11,'(5X'' INTEGER I'')')
      WRITE(11,'(5X'' REAL*8 P,A,RHO,X,T,RHOI'')')
      WRITE(11,'(5X'' REAL PP(NN)'')')
      WRITE(11,'(5X'' DATA PP'')')
      WRITE(11,'(5X''* /'')')
      DO K=1,NRHO/4-1
       WRITE(11,104) (A(RHO(L+4*(K-1)),X,T),L=1,4)
      END DO
      K=NRHO/4
      WRITE(11,105) (A(RHO(L+4*(K-1)),X,T),L=1,4)
      WRITE(11,'(5X''* /'')')
      WRITE(11,'(5X'' IF(RHO.LE.0.D0)THEN'')')
      WRITE(11,'(5X''  P=PP(1)'')')
      WRITE(11,'(5X'' ELSE IF(RHO.GE.RHOMAX)THEN'')')
      WRITE(11,'(5X''  P=PP(NN)'')')
      WRITE(11,'(5X'' ELSE'')')
      WRITE(11,'(5X''  I=INT(RHO/DELTARHO)+1'')')
      WRITE(11,'(5X''  RHOI=RHOMAX*(I-1.D0)/(NN-1.D0)'')')
      WRITE(11,'(5X''  P=PP(I)+(RHO-RHOI)*(PP(I+1)-PP(I))/DELTARHO'')')
      WRITE(11,'(5X'' END IF'')')
      WRITE(11,'(5X'' A=P'')')
      WRITE(11,'(5X'' RETURN'')')
      WRITE(11,'(5X'' END'')')

      WRITE(11,'('''')')
      WRITE(11,'(''C***********************************************'')')
      WRITE(11,'(5X'' FUNCTION V0(RHO,X,T)'')')
      WRITE(11,'(5X'' IMPLICIT NONE'')')
      WRITE(11,'(5X'' INTEGER NN'')')
      WRITE(11,'(5X'' PARAMETER ( NN='',I4,'' )'')') NRHO
      WRITE(11,'(5X'' REAL*8 RHOMAX'')')
      WRITE(11,'(5X'' PARAMETER ( RHOMAX='',E13.7,'' )'')') RHOMAX
      WRITE(11,'(5X'' REAL*8 DELTARHO'')')
      WRITE(11,'(5X'' PARAMETER ( DELTARHO='',E13.7,'' )'')') DELTARHO
      WRITE(11,'(5X'' INTEGER I'')')
      WRITE(11,'(5X'' REAL*8 P,V0,RHO,X,T,RHOI'')')
      WRITE(11,'(5X'' REAL PP(NN)'')')
      WRITE(11,'(5X'' DATA PP'')')
      WRITE(11,'(5X''* /'')')
      DO K=1,NRHO/4-1
       WRITE(11,104) (V0(RHO(L+4*(K-1)),X,T),L=1,4)
      END DO
      K=NRHO/4
      WRITE(11,105) (V0(RHO(L+4*(K-1)),X,T),L=1,4)
      WRITE(11,'(5X''* /'')')
      WRITE(11,'(5X'' IF(RHO.LE.0.D0)THEN'')')
      WRITE(11,'(5X''  P=PP(1)'')')
      WRITE(11,'(5X'' ELSE IF(RHO.GE.RHOMAX)THEN'')')
      WRITE(11,'(5X''  P=PP(NN)'')')
      WRITE(11,'(5X'' ELSE'')')
      WRITE(11,'(5X''  I=INT(RHO/DELTARHO)+1'')')
      WRITE(11,'(5X''  RHOI=RHOMAX*(I-1.D0)/(NN-1.D0)'')')
      WRITE(11,'(5X''  P=PP(I)+(RHO-RHOI)*(PP(I+1)-PP(I))/DELTARHO'')')
      WRITE(11,'(5X'' END IF'')')
      WRITE(11,'(5X'' V0=P'')')
      WRITE(11,'(5X'' RETURN'')')
      WRITE(11,'(5X'' END'')')

      PRINT*,' '
      PRINT*,' Finish '
      PRINT*,' '
      CLOSE(11)
  104 FORMAT(5X'*'4(E13.7","))
  105 FORMAT(5X'*'3(E13.7",")E13.7)

      STOP
  620 PRINT*,' I/O error. File ',FILE
      STOP
      END

C**********************************************************************
      FUNCTION A0(U,X,T)
      IMPLICIT NONE
      REAL*8 DELTA
      PARAMETER ( DELTA=1D-5 )
      REAL*8 A0,U,X,T
      REAL*8 FLUX,UMIN,AMAX,AMIN
      REAL*8 UMAX
      COMMON/RHOMAX/ UMAX
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      UMIN=0.D0
      AMAX=(FLUX(UMAX      ,X,T)-FLUX(UMAX-DELTA,X,T))/DELTA
      AMIN=(FLUX(UMIN+DELTA,X,T)-FLUX(UMIN      ,X,T))/DELTA
      IF(U+DELTA.GT.UMAX)THEN
       A0=AMAX
      ELSE IF(U-DELTA.LT.0.D0)THEN
       A0=AMIN
      ELSE
       A0=(FLUX(U+DELTA,X,T)-FLUX(U-DELTA,X,T))/2./DELTA
      END IF
      RETURN
      END

C**********************************************************************
      FUNCTION V0(U,X,T)
      IMPLICIT NONE
      REAL*8 ZERO
      PARAMETER ( ZERO=1D-10 )
      REAL*8 V0,U,X,T
      REAL*8 FLUX,UU
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      UU=U
      IF(UU.LE.ZERO)UU=ZERO
      V0=FLUX(UU,X,T)/UU
      RETURN
      END

C**********************************************************************
      FUNCTION A(U,X,T)
      IMPLICIT NONE
      REAL*8 A,U,X,T
      REAL*8 V0,A0
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      A=V0(U,X,T)-A0(U,X,T)
      RETURN
      END

C**********************************************************************
      FUNCTION P(U,X,T)
      IMPLICIT NONE
      REAL*8 ZERO,DU
      PARAMETER ( ZERO=1D-10,DU=1D-3 )
      INTEGER L,NSUM
      REAL*8 P,U,X,T
      REAL*8 A,F1,F2,S1,S2,SMAX,SMIN,DDU
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      NSUM=INT(U/DU)
      SMIN=0.D0
      SMAX=U
      DDU=SMIN+(SMAX-SMIN)/(NSUM-1.)
      IF(U.LE.ZERO.OR.NSUM.LT.2)THEN
       P=0D0
      ELSE
       P=0.D0
       S1=0.D0
       F1=0.D0
       DO L=1,NSUM-1
        S2=SMIN+(SMAX-SMIN)*((L+1)-1)/(NSUM-1.)
        F2=A(S2,X,T)**2
        P=P+(F1+F2)*DDU/2.
        S1=S2
        F1=F2
       END DO
      END IF
      RETURN
      END

C**********************************************************************
      FUNCTION H(U,X,T)
      IMPLICIT NONE
      REAL*8 ZERO,DU
      PARAMETER ( ZERO=1D-10,DU=1D-3 )
      INTEGER L,NSUM
      REAL*8 H,U,X,T
      REAL*8 A,F1,F2,S1,S2,SMAX,SMIN,DDU
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      NSUM=INT(U/DU)
      SMIN=0.D0
      SMAX=U
      DDU=SMIN+(SMAX-SMIN)/(NSUM-1.)
      IF(U.LE.ZERO.OR.NSUM.LT.2)THEN
       H=0D0
      ELSE
       H=0.D0
       S1=0.D0
       F1=0.D0
       DO L=1,NSUM-1
        S2=SMIN+(SMAX-SMIN)*((L+1)-1)/(NSUM-1.)
        F2=A(S2,X,T)/S2
        H=H+(F1+F2)*DDU/2.
        S1=S2
        F1=F2
       END DO
      END IF
      RETURN
      END

