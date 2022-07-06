!----------------------------------------------------------------------
      program snoedrvr 
!----------------------------------------------------------------------

      use snoe, only: snoe_init, snoe_zm

      implicit none

      integer :: doy
      real :: kp, f107
      integer :: i

      integer, parameter :: nmlat = 33
      integer, parameter :: nlev = 16

      real, dimension(nlev) :: alt
      real, dimension(nmlat) :: mlat 
      real, dimension(nmlat, nlev):: zm 

      integer :: nalt

!... get model parameters

      call rd_params( doy, kp, f107 )

      call snoe_init()

      call snoe_zm(doy, kp, f107, zm, mlat, alt)

      print *, 'model NO at ', alt(13)
      print *, zm(:,13) 

      call write_snoe( doy, kp, f107, mlat, alt, zm)

      end program snoedrvr
      
!----------------------------------------------------------------------
      subroutine rd_params( doy, kp, f107 )
!----------------------------------------------------------------------
      
      implicit none

      integer :: doy
      real :: kp, f107

      print *, 'enter day of year:'
      read (*,*) doy 
      print *, 'enter Kp index' 
      read (*,*) kp 
      print *, 'enter F10.7 index:'
      read (*,*) f107 

      print *, doy, kp, f107

      end subroutine rd_params

!----------------------------------------------------------------------
      subroutine write_snoe( doy, kp, f107, mlat, alt, zm)
!----------------------------------------------------------------------
!... write out results
!----------------------------------------------------------------------

      implicit none

      include 'netcdf.inc'

      integer, parameter :: nmlat = 33
      integer, parameter :: nlev = 16

      integer :: doy
      real :: kp, f107

      real, dimension(nmlat) :: mlat
      real, dimension(nlev) :: alt

      real, dimension(nmlat, nlev):: zm 
      integer :: istat

      integer :: ncid
      integer :: mlat_dim, lev_dim
      integer :: mlat_id, alt_id, zm_id 

      istat = nf_create( 'snoezm.nc', NF_CLOBBER, ncid)
      call check_err(istat)

!... define dimensions

      istat = nf_def_dim(ncid, 'mlat', nmlat, mlat_dim)
      call check_err(istat)

      istat = nf_def_dim(ncid, 'lev', nlev, lev_dim)
      call check_err(istat)    

!... define variables

      istat = nf_def_var(ncid, 'mlat', NF_REAL, 1, mlat_dim, mlat_id)
      call check_err(istat)

      istat = nf_def_var(ncid, 'alt', NF_REAL, 1, lev_dim, alt_id)
      call check_err(istat)

      istat = nf_def_var(ncid, 'zm', NF_REAL, 2,                      &
                         (/mlat_dim, lev_dim/), zm_id)
      call check_err(istat)

      istat = nf_put_att_int(ncid, NF_GLOBAL, 'DOY', NF_INT, 1, doy)
      call check_err(istat)

      istat = nf_put_att_real(ncid, NF_GLOBAL, 'KP', NF_REAL, 1, kp)
      call check_err(istat)

      istat = nf_put_att_real(ncid, NF_GLOBAL, 'F107', NF_REAL, 1, f107)
      call check_err(istat)

!... leave define mode

      istat = nf_enddef(ncid)
      call check_err(istat)

      istat = nf_put_var_real(ncid, mlat_id, mlat)
      call check_err(istat)
      istat = nf_put_var_real(ncid, alt_id, alt)
      call check_err(istat)
      istat = nf_put_var_real(ncid, zm_id, zm)
      call check_err(istat)

      istat = nf_close( ncid)
      call check_err(istat)

!----------------------------------------------------------------------
      end subroutine write_snoe
!----------------------------------------------------------------------

!----------------------------------------------------------------------
      subroutine check_err(istat)
!----------------------------------------------------------------------

      implicit none

      include 'netcdf.inc'

      integer istat

      if (istat .ne. NF_NOERR) then
        print *, nf_strerror(istat)
        stop
      endif

      end subroutine check_err
