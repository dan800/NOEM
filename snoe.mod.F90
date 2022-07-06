!----------------------------------------------------------------------
      module snoe
!----------------------------------------------------------------------
!
! An empirical model of nitric oxide (NO) in the lower thermosphere
! (100 - 150 km altitude), based on measurements from the Student
! Nitric Oxide Explorer (SNOE). Model uses empirical orthogonal functions
! (EOFs) derived from the SNOE dataset to describe spatial variability
! in NO. Model NO is the sum of a mean distribution and EOFs multiplied
! by coefficients based on geophysical parameters. These geophysical
! parameters are day of year, Kp magnetic index and F10.7 solar uv index.
!
! Model is utilized by calling subroutine snoe_zm(), which returns
! a 2-D zonal mean distribution of NO on geomagnetic coordinates. 
! Altitude is fixed to SNOE grid (every 3.33 km).
!
! Marsh, D. R., S. C. Solomon, and A. E. Reynolds (2004), Empirical model
! of nitric oxide in the lower thermosphere, J. Geophys. Res., 109, 
! A07301, doi:10.1029/2003JA010199.
!
!----------------------------------------------------------------------

      implicit none

      private

      public :: snoe_init, snoe_zm

      integer, parameter :: nmodes = 3
      integer, parameter :: nlev = 16
      integer, parameter :: nmlat = 33 

!... snoe mean and eof data

      real, dimension(nlev) :: lev
      real, dimension(nmlat) :: mlat 
      real, dimension(nmlat, nlev) :: no_mean 
      real, dimension(nmlat, nlev, nmodes) :: eofs 

      logical :: debug = .true.

      save

      contains
	
!----------------------------------------------------------------------
      subroutine snoe_init()
!----------------------------------------------------------------------

!... read snoe netcdf data

     call snoe_rdeof()

     end subroutine snoe_init

!----------------------------------------------------------------------
      subroutine snoe_zm(doy, kp, f107, zm, lat, alt)
!----------------------------------------------------------------------
!
!... calculates zonal mean nitric oxide distribution on a given day 
!... and solar conditions (represented by the f10.7 and kp indices)
!
!----------------------------------------------------------------------

      implicit none

      real, parameter :: pi = 3.1415926

      integer, intent(in) :: doy
      real,    intent(in) :: kp 
      real,    intent(in) :: f107
      real,    intent(out), dimension(nmlat) :: lat 
      real,    intent(out), dimension(nlev)  :: alt 
      real,    intent(out), dimension(nmlat, nlev) :: zm

      real :: theta0         ! day number in radians 
      real :: dec            ! solar declination angle
      real :: m1, m2, m3     ! coefficients for first 3 eofs


      if (debug) print *, 'snoe_zm(doy, kp, f107)', doy, kp, f107

!... calculate coefficients (m1 to m3) for eofs based on geophysical parametes
!... eof1 - kp 

      m1 =  kp * 0.689254 - 1.53366

!... eof2 - declination

      theta0 = 2. * pi * float(doy - 1) / 365.

      dec = 0.006918                                               &
          - 0.399912 * cos(theta0)   + 0.070257 * sin(theta0)    &
          - 0.006758 * cos(2*theta0) + 0.000907 * sin(2*theta0)  &
          - 0.002697 * cos(3*theta0) + 0.001480 * sin(3*theta0)

      dec = dec * 180./3.1415927

      if (debug) print *, 'dec', dec

      m2 = -0.31978                                                &
         + dec    * 0.097309                                       &
         + dec**2 * 0.00048979                                     &
         - dec**3 * 0.00010360
      
!... eof3 - f107 

      m3 =  alog10(f107) * 6.35777 - 13.8163 

!... zonal mean distrib. is sum of mean and eofs

      if (debug) print *, 'm1,2,3: ', m1, m2, m3 

      zm(:,:) = no_mean(:,:)                                       &
              - m1 * eofs(:,:,1)                                   &
              + m2 * eofs(:,:,2)                                   &
              - m3 * eofs(:,:,3) 

      alt = lev
      lat = mlat

      return

      end subroutine snoe_zm 

!----------------------------------------------------------------------
      subroutine snoe_rdeof()
!----------------------------------------------------------------------
!
! read in eofs/pcs from netcdf file
!
!----------------------------------------------------------------------

      implicit none

      include 'netcdf.inc'

      integer :: istat
      integer :: ncid, var_id 

      if (debug) print *, 'snoe_rdeof()'

      istat = nf_open( 'noem_eof.nc', NF_NOWRITE, ncid)
      call check_err(istat)

      istat = nf_inq_varid(ncid, 'NO', var_id)
      
      istat = nf_get_vara_real(ncid, var_id,                       &
                   (/1,1/), (/nmlat, nlev/), no_mean)
      call check_err(istat)

      istat = nf_inq_varid(ncid, 'EOF', var_id)
      
      istat = nf_get_vara_real(ncid, var_id,                       &
                   (/1,1,1/), (/nmlat, nlev, nmodes/), eofs)
      call check_err(istat)

      istat = nf_inq_varid(ncid, 'lat', var_id)
      istat = nf_get_vara_real(ncid, var_id, 1, nmlat, mlat)
      call check_err(istat)

      istat = nf_inq_varid(ncid, 'z', var_id)
      istat = nf_get_vara_real(ncid, var_id, 1, nlev, lev)
      call check_err(istat)

      istat = nf_close( ncid)
      call check_err(istat)

      return
      end subroutine snoe_rdeof

!----------------------------------------------------------------------
      subroutine check_err(istat)
!----------------------------------------------------------------------
!
!... routine to print netcdf error messages if an error occurs during a
!... call to netcdf functions
!
!----------------------------------------------------------------------
      implicit none

      include 'netcdf.inc'

      integer istat

      if (istat .ne. NF_NOERR) then
        print *, 'check_err():', nf_strerror(istat)
        stop
      endif

      return
      end subroutine check_err
      
!----------------------------------------------------------------------
      end module snoe
!----------------------------------------------------------------------

