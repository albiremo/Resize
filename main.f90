
PROGRAM main

USE resize

!---------------------------- dichiaro--------------------------- !
INTEGER :: nx,ny,nz,nxn,nzn
INTEGER  :: ix, IIX, IXold,iz,IIZ,IZold,iV,ii,iy !divisione del dominio
REAL(KIND=8), PARAMETER  :: NNx = 1, NNz = 1
REAL(KIND=8) :: alfa0,alfa0n,beta0n,ni,a,ymin,ymax, time, timen
COMPLEX, allocatable :: V(:,:,:,:)
COMPLEX, allocatable :: Vn(:,:,:,:)
character(len=40) :: filename

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


DO ix = 1, nx, 1
  IIX = ix+1
  IXold = FLOOR(ix*NNx) + 1
  WRITE(*,*) "computing new"
  DO iz = -nzn,nzn,1
    IIZ = iz + 1 + nzn;     IZold = FLOOR(iz*NNz) + 1 + nz
    DO iV=1,3
        DO iy=1,ny+3
          Vn(iy,IIZ,IIX,iV) = V(iy,IZold,IXold,iV)
        END DO
    END DO
  END DO
END DO
! salvataggio
WRITE(*,*) "Writing Dati.cart.out NEW"
filename="Dati.cart2.out"; CALL save_restart_file(filename)

END PROGRAM main
