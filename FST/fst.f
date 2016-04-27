      IMPLICIT NONE

      REAL*8 PI
      PARAMETER( PI=3.1415927 )

      INTEGER NNF,NNX,NNY
      PARAMETER(NNF=2,   !##
     *          NNX=15000,
     *          NNY=1)

      CHARACTER*25 FILEOUT,FILE,FILEIN

      INTEGER 
     *        NF,NX,NY,
     *        LENGTHOUT,LENGTHIN,
     *        NSTEPS,NOUT,NPAR,NOP,NINIT,
     *        I,N,L,LL,II
      REAL*8 XMIN,DELT,CPU,TINIT
      REAL*8 UO,VO,XO
      REAL*8 INIT
      REAL*8 
     *       X(NNX),
     *       Y(NNY),
     *       U(NNX,5),
     *       V(NNX,5),
     *       UU(NNX,NNY,5,NNF),
     *       T(5)

      REAL*8  XOUT(9)
      INTEGER IOUT(9)

      INTEGER NUM_1,NUM_2,NUM_3,NUM_4,NUM_5,NUM_6,NT
      CHARACTER*1 CHAR(0:10)
      DATA CHAR /'0','1','2','3','4','5','6','7','8','9','~'/

      COMMON /FIELD/ UU
      COMMON /TT/ T
      COMMON /XX/ X
      COMMON /YY/ Y
      COMMON /NFXY/ NF,NX,NY

      EQUIVALENCE 
     *           (UU(1,1,1,1),U),
     *           (UU(1,1,1,2),V)

      REAL*8 PAR(9)
      COMMON/USER_PARAM/ PAR

      REAL*8  BETA,TAU,XMAX
      COMMON/REAL_PARAM/ BETA,TAU,XMAX

c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      NF=NNF
      NY=NNY

      READ*,FILE

      print*,' '
      print*,' Reading initialization file ',FILE
      print*,' '
C***********
      OPEN  (1,FILE=FILE,STATUS='OLD',FORM='FORMATTED',ERR=620)
C
      READ  (1,*) FILEOUT
      READ  (1,*) FILEIN
c----------------
      READ  (1,*) NINIT
      READ  (1,*) NOUT
      READ  (1,*) NSTEPS
      READ  (1,*) NX
c----------------
      READ  (1,*) TINIT
      READ  (1,*) DELT
      READ  (1,*) XMAX
c----------------
      READ  (1,*) BETA
      READ  (1,*) TAU
c----------------
      READ  (1,*) NPAR
       DO I=1,NPAR
        READ  (1,*) PAR(I)
       END DO
c----------------
      READ  (1,*) NOP
       DO I=1,NOP
        READ  (1,*) XOUT(I)
       END DO
C
      CLOSE (1)
C
      WRITE(*,'('' FILEOUT = '',A25)'  ) FILEOUT
      WRITE(*,'('' FILEIN  = '',A25)'  ) FILEIN
c----------------
      WRITE(*,'('' NINIT   = '',I6)'   ) NINIT
      WRITE(*,'('' NOUT    = '',I6)'   ) NOUT
      WRITE(*,'('' NSTEPS  = '',I6)'   ) NSTEPS
      WRITE(*,'('' NX      = '',I6)'   ) NX
c----------------
      WRITE(*,'('' DELT    = '',D13.7)') TINIT
      WRITE(*,'('' DELT    = '',D13.7)') DELT
      WRITE(*,'('' XMAX    = '',D13.7)') XMAX
c----------------
      WRITE(*,'('' BETA    = '',D13.7)') BETA
      WRITE(*,'('' TAU     = '',D13.7)') TAU
c----------------
      WRITE(*,'('' NPAR    = '',I6)'   ) NPAR
       DO I=1,NPAR
        WRITE(*,'(''  PAR(''I1'')  = '',D13.7)') I,PAR(I)
       END DO
c----------------
      WRITE(*,'('' NOP     = '',I6)'   ) NOP
       DO I=1,NOP
        WRITE(*,'(''  XOUT(''I1'') = '',D13.7)') I,XOUT(I)
       END DO
C
C**********


      IF(NX.GT.NNX)THEN
       PRINT*,' "NX" should be less than ',NNX
       STOP
      END IF

      DO I=1,25
       IF(FILEOUT(I:I).EQ.' '.OR.FILEOUT(I:I+1).EQ.'.d')THEN
        LENGTHOUT=I-1
        GO TO 9192
       END IF
      END DO
 9192 CONTINUE

      DO I=1,25
       IF(FILEIN(I:I).EQ.' '.OR.FILEIN(I:I+1).EQ.'.d')THEN
        LENGTHIN=I-1
        GO TO 9193
       END IF
      END DO
 9193 CONTINUE

      XMIN=0.
      DO I=1,NX
       X(I)=XMIN+(XMAX-XMIN)*(I-1.D0)/(NX-1.D0)
      END DO

      print*,' '
      DO L=1,NOP
       LL=20+L
       IOUT(L)=1
       DO I=2,NX-1
        IF(XOUT(L).GE.X(I).AND.XOUT(L).LT.X(I+1)) IOUT(L)=I
       END DO
       NT=NINT(XOUT(L)*1000)
       NUM_1= NT/100000
       NUM_2=(NT-NUM_1*100000)/10000
       NUM_3=(NT-NUM_1*100000-NUM_2*10000)/1000
       NUM_4=(NT-NUM_1*100000-NUM_2*10000-NUM_3*1000)/100
       NUM_5=(NT-NUM_1*100000-NUM_2*10000-NUM_3*1000-NUM_4*100)/10
       NUM_6=(NT-NUM_1*100000-NUM_2*10000-NUM_3*1000-NUM_4*100-NUM_5*10)
       IF(NUM_1.GT.9)NUM_1=10
       IF(NUM_2.GT.9)NUM_2=10
       IF(NUM_3.GT.9)NUM_3=10
       IF(NUM_4.GT.9)NUM_4=10
       IF(NUM_5.GT.9)NUM_5=10
       IF(NUM_6.GT.9)NUM_6=10
       FILE=FILEOUT(1:LENGTHOUT)//'_'//'x'//'_'//
     *     CHAR(NUM_1)//CHAR(NUM_2)//CHAR(NUM_3)//'_'//
     *     CHAR(NUM_4)//CHAR(NUM_5)//CHAR(NUM_6)//'.dat'
      WRITE(*,'(1X,1I4'' IOUT=''1I4'' XOUT=''E10.4'' FILE: ''A25)')
     *             L,    IOUT(L),      XOUT(L),        FILE
       OPEN(LL,FILE=FILE,FORM='FORMATTED',STATUS='UNKNOWN',ERR=620)
       REWIND(LL)
      END DO
      print*,' '

c---<*>-------------< Initial conditions >---------------------------<1>
      T(5)=TINIT-0*DELT
      T(4)=TINIT-1*DELT
      IF(NINIT.EQ.0)THEN
       DO I=1,NX
        DO L=4,5
         U(I,L)=INIT(1,X(I))
         V(I,L)=INIT(2,X(I))
        END DO
       END DO
      ELSE
C***********
       FILE=FILEIN(1:LENGTHIN)//'.dat'
       PRINT*,' Reading initial data from ',FILE
       OPEN(11,FILE=FILE,FORM='FORMATTED',STATUS='OLD',ERR=620)
       DO I=1,NX
         READ(11,105) X(I),UO,VO
         DO L=4,5
          U(I,L)=UO
          V(I,L)=VO
         END DO
       END DO
       CLOSE(11)
C***********
      END IF
  105 FORMAT(100(1X,E14.8))
c---<*>--------------------------------------------------------------<2>

      print*,' '
      print*,' Let''s go '
      print*,' '


C***********
       IF(T(5).LT.1.D+5)NT=NINT(T(5)*1000)
       IF(T(5).GT.1.D+5)NT=NINT(1.D+5*1000)
       NUM_1= NT/100000
       NUM_2=(NT-NUM_1*100000)/10000
       NUM_3=(NT-NUM_1*100000-NUM_2*10000)/1000
       NUM_4=(NT-NUM_1*100000-NUM_2*10000-NUM_3*1000)/100
       NUM_5=(NT-NUM_1*100000-NUM_2*10000-NUM_3*1000-NUM_4*100)/10
       NUM_6=(NT-NUM_1*100000-NUM_2*10000-NUM_3*1000-NUM_4*100-NUM_5*10)
       IF(NUM_1.GT.9)NUM_1=10
       IF(NUM_2.GT.9)NUM_2=10
       IF(NUM_3.GT.9)NUM_3=10
       IF(NUM_4.GT.9)NUM_4=10
       IF(NUM_5.GT.9)NUM_5=10
       IF(NUM_6.GT.9)NUM_6=10
C***********
      FILE=FILEOUT(1:LENGTHOUT)//'_'//'t'//'_'//
     *     CHAR(NUM_1)//CHAR(NUM_2)//CHAR(NUM_3)//'_'//
     *     CHAR(NUM_4)//CHAR(NUM_5)//CHAR(NUM_6)//'.dat'
c
      WRITE(*,'(1X,1I6''  T= ''E10.4''  CPU= ''E10.4''  FILE: ''A25)')
     *             0,          0.D0,           0.D0,            FILE
c
      OPEN(11,FILE=FILE,FORM='FORMATTED',STATUS='UNKNOWN',ERR=620)
      REWIND(11)
      DO I=1,NX
        UO=U(I,5)
        VO=V(I,5)
        WRITE(11,104) X(I),UO,VO
      END DO
      CLOSE(11)
C***********

c---<*>-------------------< MAIN CYCLE >-----------------------------<1>
      DO N=1,NSTEPS

       T(1)=T(2)
       T(2)=T(3)
       T(3)=T(4)
       T(4)=T(5)
       T(5)=T(5)+DELT

       CALL SOLVER
       CALL SECOND(CPU)

       DO L=1,NOP
        LL=20+L
        II=IOUT(L)
        XO=XOUT(L)
        UO=U(II,5)+(XO-X(II))*(U(II+1,5)-U(II,5))/(X(II+1)-X(II))
        VO=V(II,5)+(XO-X(II))*(V(II+1,5)-V(II,5))/(X(II+1)-X(II))
        WRITE(LL,104) T(5),UO,VO
       END DO

      IF(1.*(N/NOUT).EQ.(1.*N)/NOUT)THEN!{1
       IF(T(5).LT.1.D+5)NT=NINT(T(5)*1000)
       IF(T(5).GT.1.D+5)NT=NINT(1.D+5*1000)
       NUM_1= NT/100000
       NUM_2=(NT-NUM_1*100000)/10000
       NUM_3=(NT-NUM_1*100000-NUM_2*10000)/1000
       NUM_4=(NT-NUM_1*100000-NUM_2*10000-NUM_3*1000)/100
       NUM_5=(NT-NUM_1*100000-NUM_2*10000-NUM_3*1000-NUM_4*100)/10
       NUM_6=(NT-NUM_1*100000-NUM_2*10000-NUM_3*1000-NUM_4*100-NUM_5*10)
       IF(NUM_1.GT.9)NUM_1=10
       IF(NUM_2.GT.9)NUM_2=10
       IF(NUM_3.GT.9)NUM_3=10
       IF(NUM_4.GT.9)NUM_4=10
       IF(NUM_5.GT.9)NUM_5=10
       IF(NUM_6.GT.9)NUM_6=10
C***********
      FILE=FILEOUT(1:LENGTHOUT)//'_'//'t'//'_'//
     *     CHAR(NUM_1)//CHAR(NUM_2)//CHAR(NUM_3)//'_'//
     *     CHAR(NUM_4)//CHAR(NUM_5)//CHAR(NUM_6)//'.dat'
c
      WRITE(*,'(1X,1I6''  T= ''E10.4''  CPU= ''E10.4''  FILE: ''A25)')
     *             N,          T(5),           CPU,             FILE
c
      OPEN(11,FILE=FILE,FORM='FORMATTED',STATUS='UNKNOWN',ERR=620)
      REWIND(11)
      DO I=1,NX
        UO=U(I,5)
        VO=V(I,5)
        WRITE(11,104) X(I),UO,VO
      END DO
      CLOSE(11)
C***********
  104 FORMAT(100(1X,E14.8))
      END IF!1}

      END DO
c---<*>--------------------------------------------------------------<2>
      CLOSE(10)

      STOP
  620 PRINT*,' I/O error. File ',FILE
      STOP
      END

C**********************************************************************

      SUBROUTINE SOLVER
      IMPLICIT NONE

      INTEGER NNF,NNX,NNY
      PARAMETER( NNF=2,   !##
     *           NNX=15000,
     *           NNY=1)

C** Parametry blja obraschalki F04AXF plus F01BRF
C     
C      NEQN=NNF*NNX*NNY
C      LICN=200*NNX*NNY ???
C      LIRN=100*NNX*NNY ???
C**
      INTEGER LICN,LIRN
      PARAMETER( LICN= 1 000 000,
     *           LIRN=   500 000)
C**
C  SUBROUTINE //F04AXF//(NEQN,X,LICN,ICN,IKEEP,RHS,W,MTYPE,IDISP,RESID) 
C**
      INTEGER NEQN,ICN(LICN),IKEEP(5*(NNF*NNX*NNY)),MTYPE
      REAL*8 X(LICN),RHS(NNF*NNX*NNY),W(NNF*NNX*NNY),RESID
C**
C     SUBROUTINE //F01BRF//(NEQN, NELEM, X, LICN, IRN, LIRN, ICN, PIVOT, 
C    1                      IKEEP, IW, W, LBLOCK, GROW, ABORT, 
C    2                      IDISP, IFAIL) 
C**
      INTEGER NELEM,IRN(LIRN),IW(8*(NNF*NNX*NNY)),IDISP(10), IFAIL 
      REAL*8  PIVOT
      LOGICAL LBLOCK, GROW, ABORT(4)
      
      DATA ABORT /.TRUE.,.TRUE.,.FALSE.,.TRUE./ 
      DATA GROW /.TRUE./ LBLOCK /.TRUE./ 

C** General 

      LOGICAL MODE
      INTEGER I,J,K,L,M,N,NN,MM,KMIN,KMAX,LMIN,LMAX,NITER,IMAX
      REAL*8 COEFF,CF1,CF2,DLT,TAU,DELTA,TAUOLD
 
C** Polja

      REAL*8 UU,T
      COMMON /FIELD/ UU(NNX,NNY,5,NNF)
      COMMON /TT/ T(5)

      INTEGER NF,NX,NY
      COMMON /NFXY/ NF,NX,NY

C** Shablony

      INTEGER NSH
      PARAMETER (NSH=17)
      INTEGER S(NSH)

c      real*8 tmp(500,500)
c      data tmp /250000*0.d0/

c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      PIVOT=.9D0
      MTYPE=1
      NEQN=NNF*NX*NY

      DLT=1.D-9

      DELTA=1.D-8
      NITER=0
      IMAX=50
      TAUOLD=100.
      TAU=100.

      DO N=1,NF
       DO I=1,NX
        DO J=1,NY
         UU(I,J,1,N)=UU(I,J,2,N)
         UU(I,J,2,N)=UU(I,J,3,N)
         UU(I,J,3,N)=UU(I,J,4,N)
         UU(I,J,4,N)=UU(I,J,5,N)
        END DO
       END DO
      END DO

 1111 CONTINUE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      MODE=.TRUE.
      NITER=NITER+1
      IF(NITER.GT.1.AND.TAUOLD.GT.TAU)MODE=.FALSE.
      IF(NITER.EQ.2*IMAX/3)MODE=.TRUE.
      TAUOLD=TAU

c---<*>-------------------< MAIN CYCLE >-----------------------------<1>
      NELEM=0
      DO N=1,NF
       DO I=1,NX
        DO J=1,NY
         NN=J+NY*(I-1)+NX*NY*(N-1)
c
         IF(MODE)THEN
c
         CALL EQN(0,S,N,I,J,CF1)
         DO M=1,NF
          KMIN=S(1)
          KMAX=S(2)
          DO K=KMIN,KMAX
           LMIN=S(2+(K-KMIN+1))
           LMAX=S(2+(KMAX-KMIN+1)+(K-KMIN+1))
           DO L=LMIN,LMAX
            MM=NN+L+K*NY+NX*NY*(M-N)
c
            UU(I+K,J+L,5,M)=UU(I+K,J+L,5,M)+DLT
            CALL EQN(1,S,N,I,J,CF2)
            UU(I+K,J+L,5,M)=UU(I+K,J+L,5,M)-DLT
            UU(I+K,J+L,5,M)=UU(I+K,J+L,5,M)-DLT
            CALL EQN(1,S,N,I,J,CF1)
            UU(I+K,J+L,5,M)=UU(I+K,J+L,5,M)+DLT
            COEFF=(CF2-CF1)/2.D0/DLT
c            tmp(nn,mm)=coeff
c
            IF(ABS(COEFF).GT.1.D-14)THEN 
             IF(MM.LT.1.OR.MM.GT.NEQN)THEN
              PRINT*,' SOLVER: nonzero element for M=',MM,
     *                                          ', N=',NN,
     *                                          ', I=',I,
     *                                          ', J=',J
              STOP
             END IF
             NELEM=NELEM+1
             X(NELEM)=COEFF
             IRN(NELEM)=NN
             ICN(NELEM)=MM         
            END IF
           END DO
          END DO
         END DO
c
         END IF 
c
         CALL EQN(1,S,N,I,J,COEFF)
         RHS(NN)=COEFF
        END DO
       END DO
      END DO
      do nn=1,neqn
c       write(13,'(200(f9.4))') (tmp(nn,mm),mm=1,neqn)
      end do
c---<*>--------------------------------------------------------------<2>

c      PRINT*,' Number of nonzero matrix elements is ',NZ

      IF(MODE)THEN
       IFAIL=110
       CALL F01BRF(NEQN, NELEM, X, LICN, IRN, LIRN, ICN, PIVOT,
     1             IKEEP, IW, W, LBLOCK, GROW, ABORT,
     2             IDISP, IFAIL)
      END IF
      CALL F04AXF(NEQN,X,LICN,ICN,IKEEP,RHS,W,MTYPE,IDISP,RESID)

      TAU=0.D0
      DO N=1,NF
       DO I=1,NX
        DO J=1,NY
         NN=J+NY*(I-1)+NX*NY*(N-1)
         UU(I,J,5,N)=UU(I,J,5,N)-RHS(NN)
         IF(ABS(UU(I,J,5,N)).LT.1.D-10)UU(I,J,5,N)=0.D0
         TAU=TAU+ABS(RHS(NN))
        END DO
       END DO
      END DO
c      WRITE (*,'(5X,I3,1X,L1,1X,D13.7)') NITER,MODE,TAU
      IF(TAU.GT.DELTA.AND.NITER.LT.IMAX)GO TO 1111 !!!!!!!!!!!!!!!!!!!!
      IF(NITER.GE.IMAX)THEN
        WRITE(*,'('' '')') ' '
        WRITE
     * (*,'('' "SOLVER" WARNING: Newton iterations did not converge: ''
     *                I2,1X,L1,1X,D10.4)') 
     *                    NITER,MODE,TAU
        WRITE(*,'('' '')') ' '
      END IF

      RETURN
      END

C**********************************************************************

      SUBROUTINE EQN(NREG,S,N,I,J,R)
      IMPLICIT NONE

      INTEGER NNF,NNX,NNY
      PARAMETER( NNF=2, !##
     *           NNX=15000,
     *           NNY=1)

      REAL*8 PI
      PARAMETER ( PI=3.141592653589793D0 )

      INTEGER NREG,N,I,J
      REAL*8 R

      REAL*8 UU,T,X
      COMMON /FIELD/ UU(NNX,NNY,5,NNF)
      COMMON /TT/ T(5)
      COMMON /XX/ X(NNX)

      INTEGER NF,NX,NY
      COMMON /NFXY/ NF,NX,NY

      REAL*8 
     *            U(NNX,5),
     *            V(NNX,5)
      EQUIVALENCE 
     *           (UU(1,1,1,1),U),
     *           (UU(1,1,1,2),V)


      INTEGER II
      REAL*8 BC,RHS,A,RQR,P,UUU,FFF,
     *       AP,AM,UP,UM,VP,VM,XM,XP,DX,
     *       U1L,U1R,U2L,U2R,F1L,F1R,F2L,F2R,F1P,F1M,F2P,F2M,
     *       DU1T,DU2T,DF1X,DF2X,DUX,DVX,DUT,DVT

      INTEGER NSH
      PARAMETER (NSH=17)
      INTEGER S(NSH)
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

c       dx/da=D1?2(X(I,J,5),A(I),1)


c---<*>---------------< EQUATIONS >----------------------------------<1>
c
      IF(I.EQ.1)THEN!{1
c>>>
c>>> Left boundary
c>>>
       IF(NREG.EQ.0)THEN!{2
        S(1)= 0
        S(2)= 1
c       ******
        S(3)= 0
        S(4)= 0
        S(5)= 0
c       ******
        S(6)= 0
        S(7)= 0
        S(8)= 0
        RETURN
       END IF!2}
       DUX=(U(I+1,5)-U(I,5))/(X(I+1)-X(I))
       DVX=(V(I+1,5)-V(I,5))/(X(I+1)-X(I))
       DUT=(U(I,5)-U(I,4))/(T(5)-T(4))
       DVT=(V(I,5)-V(I,4))/(T(5)-T(4))
       IF(N.EQ.1) R=BC(1,U(I,5),V(I,5),DUX,DVX,DUT,DVT,X(I),T(5))
       IF(N.EQ.2) R=BC(2,U(I,5),V(I,5),DUX,DVX,DUT,DVT,X(I),T(5))
c<<<
      ELSE IF(I.EQ.NX)THEN!1}{1
c>>>
c>>> Right boundary
c>>>
       IF(NREG.EQ.0)THEN!{2
        S(1)=-1
        S(2)= 0
c       ******
        S(3)= 0
        S(4)= 0
        S(5)= 0
c       ******
        S(6)= 0
        S(7)= 0
        S(8)= 0
        RETURN
       END IF!2}
       DUX=(U(I,5)-U(I-1,5))/(X(I)-X(I-1))
       DVX=(V(I,5)-V(I-1,5))/(X(I)-X(I-1))
       DUT=(U(I,5)-U(I,4))/(T(5)-T(4))
       DVT=(V(I,5)-V(I,4))/(T(5)-T(4))
       IF(N.EQ.1) R=BC(1,U(I,5),V(I,5),DUX,DVX,DUT,DVT,X(I),T(5))
       IF(N.EQ.2) R=BC(2,U(I,5),V(I,5),DUX,DVX,DUT,DVT,X(I),T(5))
c<<<
      ELSE!1}{1
c>>>
c>>> Main field
c>>>
       IF(NREG.EQ.0)THEN!{2
        S( 1)=-1
        S( 2)= 1
c       ******
        S( 3)= 0
        S( 4)= 0
        S( 5)= 0
c       ******
        S( 6)= 0
        S( 7)= 0
        S( 8)= 0
c       ******
        S( 9)= 0
        S(10)= 0
        S(11)= 0
        RETURN
       END IF!2}

       XM=(X(I-1)+X(I  ))/2.
       XP=(X(I  )+X(I+1))/2.
       DX=XP-XM

       UM=(U(I-1,5)+U(I  ,5))/2.
       UP=(U(I  ,5)+U(I+1,5))/2.

       VM=(V(I-1,5)+V(I  ,5))/2.
       VP=(V(I  ,5)+V(I+1,5))/2.

       AM=A(UM,XM,T(5))
       AP=A(UP,XP,T(5))

       DU1T= ( UUU(1,U(I,5),V(I,5),X(I),T(5))-
     -         UUU(1,U(I,4),V(I,4),X(I),T(4)) )/
     /                  ( T(5)-T(4) )
       DU2T= ( UUU(2,U(I,5),V(I,5),X(I),T(5))-
     -         UUU(2,U(I,4),V(I,4),X(I),T(4)) )/
     /                  ( T(5)-T(4) )

       II=I-1
c
       F1L=FFF(1,U(II  ,5),V(II  ,5),X(II  ),T(5))
       F2L=FFF(2,U(II  ,5),V(II  ,5),X(II  ),T(5))
       F1R=FFF(1,U(II+1,5),V(II+1,5),X(II+1),T(5))
       F2R=FFF(2,U(II+1,5),V(II+1,5),X(II+1),T(5))
c
       U1L=UUU(1,U(II  ,5),V(II  ,5),X(II  ),T(5))
       U2L=UUU(2,U(II  ,5),V(II  ,5),X(II  ),T(5))
       U1R=UUU(1,U(II+1,5),V(II+1,5),X(II+1),T(5))
       U2R=UUU(2,U(II+1,5),V(II+1,5),X(II+1),T(5))
c

       F1M=(F1L+F1R
     -  -( RQR(1,1,UM,VM,XM,T(5))*(U1R-U1L)
     +  +  RQR(1,2,UM,VM,XM,T(5))*(U2R-U2L) ))/2.
       F2M=(F2L+F2R
     -  -( RQR(2,1,UM,VM,XM,T(5))*(U1R-U1L)
     +  +  RQR(2,2,UM,VM,XM,T(5))*(U2R-U2L) ))/2.

       II=I
c
       F1L=FFF(1,U(II  ,5),V(II  ,5),X(II  ),T(5))
       F2L=FFF(2,U(II  ,5),V(II  ,5),X(II  ),T(5))
       F1R=FFF(1,U(II+1,5),V(II+1,5),X(II+1),T(5))
       F2R=FFF(2,U(II+1,5),V(II+1,5),X(II+1),T(5))
c
       U1L=UUU(1,U(II  ,5),V(II  ,5),X(II  ),T(5))
       U2L=UUU(2,U(II  ,5),V(II  ,5),X(II  ),T(5))
       U1R=UUU(1,U(II+1,5),V(II+1,5),X(II+1),T(5))
       U2R=UUU(2,U(II+1,5),V(II+1,5),X(II+1),T(5))
c
       F1P=(F1L+F1R
     -  -( RQR(1,1,UP,VP,XP,T(5))*(U1R-U1L)
     +  +  RQR(1,2,UP,VP,XP,T(5))*(U2R-U2L) ))/2.
       F2P=(F2L+F2R
     -  -( RQR(2,1,UP,VP,XP,T(5))*(U1R-U1L)
     +  +  RQR(2,2,UP,VP,XP,T(5))*(U2R-U2L) ))/2.

       DF1X=(F1P-F1M)/DX
       DF2X=(F2P-F2M)/DX

       IF(N.EQ.1)THEN!2{
        R=DU1T+DF1X-RHS(1,U(I,5),V(I,5),X(I),T(5))
       ELSE IF(N.EQ.2)THEN!2}{2
        R=DU2T+DF2X-RHS(2,U(I,5),V(I,5),X(I),T(5))
       END IF!2}
c<<<
      END IF!1}
c---<*>--------------------------------------------------------------<2>
      RETURN
      END

C**********************************************************************
      FUNCTION RHS(N,R,V,X,T)
      IMPLICIT NONE
      REAL*8 ZERO
      PARAMETER ( ZERO=1.D-10 )
      INTEGER N
      REAL*8 RHS,R,V,X,T,V0,RR,FLX,H,A,HH,FRC
      REAL*8 BETA,TAU,XMAX
      COMMON/REAL_PARAM/ BETA,TAU,XMAX
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RR=R
      IF(R.LE.ZERO)RR=ZERO
      HH=(1.-BETA)*(H(RR,X,T)+A(RR,X,T))
      IF(N.EQ.1)THEN
       RHS=FLX(X,T)
      ELSE IF(N.EQ.2)THEN
       RHS=RR*(V0(RR,X,T)-V)/TAU+(V+HH)*FLX(X,T)+RR*FRC(X,T)
      ELSE
       PRINT*,' RHS: "N" can''t be ',N
       STOP
      END IF
      RETURN
      END

C**********************************************************************
      FUNCTION UUU(N,R,V,X,T)
      IMPLICIT NONE
      INTEGER N
      REAL*8 UUU,R,V,X,T
      REAL*8 A,H,HH
      REAL*8 BETA,TAU,XMAX
      COMMON/REAL_PARAM/ BETA,TAU,XMAX
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF(N.EQ.1)THEN
       UUU=R
      ELSE IF(N.EQ.2)THEN
       HH=(1.-BETA)*(H(R,X,T)+A(R,X,T))
       UUU=R*( V-(1.-BETA)*A(R,X,T)+HH )
      ELSE
       PRINT*,' UUU: "N" can''t be ',N
       STOP
      END IF
      RETURN
      END

C**********************************************************************
      FUNCTION FFF(N,R,V,X,T)
      IMPLICIT NONE
      INTEGER N
      REAL*8 FFF,R,V,X,T
      REAL*8 A,H,P,HH
      REAL*8 BETA,TAU,XMAX
      COMMON/REAL_PARAM/ BETA,TAU,XMAX
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF(N.EQ.1)THEN
       FFF=R*V
      ELSE IF(N.EQ.2)THEN
       HH=(1.-BETA)*(H(R,X,T)+A(R,X,T))
       FFF=R*V*(V-(1.-BETA)*A(R,X,T)+HH)+BETA*P(R,X,T)
      ELSE
       PRINT*,' FFF: "N" can''t be ',N
       STOP
      END IF
      RETURN
      END

C**********************************************************************
      FUNCTION RQR(N,M,R,V,X,T)
      IMPLICIT NONE
      REAL*8 ZERO,K
      PARAMETER ( ZERO=1D-10,K=3D0 )
      INTEGER N,M
      REAL*8 RQR,R,V,X,T
      REAL*8 A,H,AA,HH
      REAL*8 DQ,Q,S
      REAL*8 BETA,TAU,XMAX
      COMMON/REAL_PARAM/ BETA,TAU,XMAX
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      Q(S)=(ABS(S)+1.D-10)/TANH(K*ABS(S)+1.D-10)
      DQ(S)=(-K*(1.d-10+s))/SINH(1.d-10+K*s)**2+1/TANH(1.d-10+K*s)
c
      AA=A(R,X,T)
      HH=(1.-BETA)*(H(R,X,T)+A(R,X,T))
c
      IF(AA.LE.ZERO)THEN
       IF(N.EQ.1.AND.M.EQ.1) RQR=-(DQ(v)*(v + hh)) + Q(v)
       IF(N.EQ.1.AND.M.EQ.2) RQR=DQ(v)
       IF(N.EQ.2.AND.M.EQ.1) RQR=-(DQ(v)*(v + hh)**2)
       IF(N.EQ.2.AND.M.EQ.2) RQR=DQ(v)*(v + hh) + Q(v)
      ELSE
       IF(N.EQ.1.AND.M.EQ.1) 
     -  RQR= ((v + beta*aa + hh)*Q(v - aa) - (v - aa + hh)*
     -       Q(v + beta*aa))/((1 + beta)*aa)
       IF(N.EQ.1.AND.M.EQ.2) 
     -  RQR= (-Q(v - aa) + Q(v + beta*aa))/((1 + beta)*aa)
       IF(N.EQ.2.AND.M.EQ.1) 
     -  RQR= ((v - aa + hh)*(v + beta*aa + hh)*
     -       (Q(v - aa) - Q(v + beta*aa)))/((1 + beta)*aa)
       IF(N.EQ.2.AND.M.EQ.2) 
     -   RQR= (-((v - aa + hh)*Q(v - aa)) + 
     -        (v + beta*aa + hh)*Q(v + beta*aa))/((1 + beta)*aa)
      END IF
      
      RETURN
      END

