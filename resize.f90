MODULE resize



CONTAINS
!--------------------------------------------------------------------------------------!
SUBROUTINE read_restart_file()

  IMPLICIT NONE

  INTEGER :: nx,ny,nz
  REAL(KIND=8) :: alfa0,beta0,ni,a,ymin,ymax,time
  COMPLEX, allocatable :: V(:,:,:,:)
  INTEGER :: iV,ix,iy,iz,io,nx_t,nz_t,ny_t,br=8,bc=16,iV_t,b1=1,b7=7,b3=3
  INTEGER :: pos
  OPEN(UNIT=100,FILE="Dati.cart.out",access="stream",status="old",action="read",iostat=io)
  nx_t=nx+1; ny_t=ny+3; nz_t=2*nz+1;
    WRITE(*,*) "Reading restart file..."
    READ(100,POS=1) nx,ny,nz,alfa0,beta0,ni,a,ymin,ymax,time
    DO iV=1,3
      pos=bc*ny_t*nz_t*nx_t+(iV-b1)*(bc*ny_t*nz_t*nx_t)+b1+(br*b7+b3*SIZEOF(nx))
      WRITE(*,*) pos
      READ(100,POS=pos) V(:,:,:,iV)
    END DO
    CLOSE(100)
END SUBROUTINE read_restart_file

!-------------------------------------------------------------------------------------!

SUBROUTINE save_restart_file(filename)

  IMPLICIT NONE

  INTEGER ::ny,nxn,nzn
  REAL(KIND=8) :: ni,a,ymin,ymax,time,alfa0n,beta0n,timen
  COMPLEX, allocatable :: Vn(:,:,:,:)
  INTEGER :: iV,ix,iy,iz,io,nx_t,nz_t,ny_t,br=8,bc=16,iV_t,b1=1,b7=7,b3=3
  INTEGER :: pos,i
  CHARACTER(len=40) :: filename
  OPEN(UNIT=100,FILE=TRIM(filename),access="stream",action="write")
  nx_t=nxn+1; ny_t=ny+3; nz_t=2*nzn+1;
  WRITE(UNIT=100,POS=1) nxn,ny,nzn,alfa0n,beta0n,ni,a,ymin,ymax,timen
      DO iV=1,3
        pos=bc*ny_t*nz_t*nx_t+(iV-b1)*(bc*ny_t*nz_t*nx_t)+b1+(br*b7+b3*SIZEOF(nxn))
        WRITE(100,POS=pos) Vn(:,:,:,iV)
      END DO
      CLOSE(100)
END SUBROUTINE save_restart_file
END MODULE resize
