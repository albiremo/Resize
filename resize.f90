MODULE resize

IMPLICIT NONE
INTEGER :: nx,ny,nz,nxn,nzn,nxd,nzd
REAL(KIND=8) :: alfa0,beta0,ni,a,ymin,ymax,time,alfa0n,beta0n,timen
REAL(KIND=8) :: meanpx,meanpz,meanflowx,meanflowz
REAL(KIND=8):: deltat, cflmax
REAL (KIND=8) :: dt_field, dt_save, t_max
logical :: time_from_restart
REAL (KIND=8) :: nstep
COMPLEX, allocatable :: V(:,:,:,:)
COMPLEX, allocatable :: Vn(:,:,:,:)

CONTAINS
!--------------------------------------------------------------------------------------!
SUBROUTINE read_dnsin()
   OPEN(15, file='dns.in')
   READ(15, *) nx, ny, nz; READ(15, *) alfa0, beta0; nxd=3*(nx+1)/2;nzd=3*nz
   READ(15, *) ni; READ(15, *) a, ymin, ymax; ni=1/ni
   READ(15, *) meanpx, meanpz; READ(15, *) meanflowx, meanflowz
   READ(15, *) deltat, cflmax, time
   READ(15, *) dt_field, dt_save, t_max, time_from_restart
   READ(15, *) nstep
   CLOSE(15)
END SUBROUTINE read_dnsin

SUBROUTINE read_restart_file()


  INTEGER :: iV,ix,iy,iz,io,nx_t,nz_t,ny_t,br=8,bc=16,iV_t,b1=1,b7=7,b3=3
  INTEGER :: pos
  OPEN(UNIT=100,FILE="Dati.cart.out",access="stream",status="old",action="read",iostat=io)
  nx_t=nx+1; ny_t=ny+3; nz_t=2*nz+1;
    WRITE(*,*) "Reading restart file..."
    READ(100,POS=1) nx,ny,nz,alfa0,beta0,ni,a,ymin,ymax,time
      pos=b1+(br*b7+b3*SIZEOF(nx))
      READ(100,POS=pos) V
    CLOSE(100)
END SUBROUTINE read_restart_file

!-------------------------------------------------------------------------------------!

SUBROUTINE save_restart_file(filename)

  INTEGER :: iV,ix,iy,iz,io,nx_t,nz_t,ny_t,br=8,bc=16,iV_t,b1=1,b7=7,b3=3
  INTEGER :: pos,i
  CHARACTER(len=40) :: filename
  OPEN(UNIT=101,FILE=TRIM(filename),access="stream",action="write")
  nx_t=nxn+1; ny_t=ny+3; nz_t=2*nzn+1;
  WRITE(UNIT=101,POS=1) nxn,ny,nzn,alfa0n,beta0n,ni,a,ymin,ymax,timen
    pos=b1+(br*b7+b3*SIZEOF(nxn))
    WRITE(101,POS=pos) Vn
    CLOSE(101)
END SUBROUTINE save_restart_file


SUBROUTINE free_memory()
    DEALLOCATE(V,Vn)
END SUBROUTINE free_memory

END MODULE resize
