
PROGRAM main

USE resize

IMPLICIT NONE
!---------------------------- dichiaro--------------------------- !
INTEGER  :: ix, IIX, IXold,iz,IIZ,IZold,iV,ii,iy !divisione del dominio
REAL(KIND=8), PARAMETER  :: NNx = 2, NNz = 1
character(len=40) :: filename
CALL read_dnsin()
ALLOCATE(V(-1:ny+1,-nz:nz,0:nx,1:3))
CALL read_restart_file()

  ! Output DNS.in
  WRITE(*,*) " "
  WRITE(*,*) "!====================================================!"
  WRITE(*,*) "!                  OLD FIELD                         !"
  WRITE(*,*) "!====================================================!"
  WRITE(*,*) " "
  WRITE(*,"(A,I5,A,I5,A,I5)") "   nx =",nx,"   ny =",ny,"   nz =",nz
  WRITE(*,"(A,F6.4,A,F6.4,A,F8.6)") "   alfa0 =",alfa0,"       beta0 =",beta0,"   ni =",ni
  WRITE(*,*) " "


! dimensione dominio
alfa0n = alfa0*NNx
beta0n = beta0*NNz

! numero di nodi
nxn = FLOOR(nx/NNX)
nzn = FLOOR(nz/NNZ)


timen = 0

  ! Output DNS.in
  WRITE(*,*) " "
  WRITE(*,*) "!====================================================!"
  WRITE(*,*) "!                  NEW FIELD                         !"
  WRITE(*,*) "!====================================================!"
  WRITE(*,*) " "
  WRITE(*,"(A,I5,A,I5,A,I5)") "   nx =",nxn,"   ny =",ny,"   nz =",nzn
  WRITE(*,"(A,F6.4,A,F6.4,A,F8.6)") "   alfa0 =",alfa0n,"       beta0 =",beta0n,"   ni =",ni
  WRITE(*,*) " "

ALLOCATE(Vn(-1:ny+1,-nzn:nzn,0:nxn,1:3))
DO ix = 0, nxn, 1
  IIX = ix+1
  IXold = FLOOR(ix*NNx) + 1
  WRITE(*,*)"calcola ix"
  !WRITE(*,"(A,I5)") "   nx =",ix
  DO iz = -nzn,nzn,1
    IIZ = iz + 1 + nzn;     IZold = FLOOR(iz*NNz) + 1 + nz
    DO iV=1,3
        DO iy=-1,ny+1
          Vn(iy,IIZ,IIX,iV) = V(iy,IZold,IXold,iV)
        END DO
    END DO
  END DO
END DO

WRITE(*,*) SHAPE(V)
! salvataggio
WRITE(*,*) "Writing Dati.cart.out NEW"
filename="Dati.cart2.out"; CALL save_restart_file(filename); CALL free_memory

END PROGRAM main
