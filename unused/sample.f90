program sample_read
! Fortran 90 version.

!   This is a simple program to write data in the WPS intermediate
!   format.  It is included mainly as an aid to understanding said 
!   format.

  implicit none

! Declarations:

  integer, parameter :: IUNIT = 10
  integer, parameter :: OUNIT = 11
  integer :: ierr

  integer :: IFV=5
  character(len=24) :: HDATE
  real :: XFCST
  character(len=8) :: STARTLOC
  character(len=9) :: FIELD
  character(len=25) :: UNITS
  character(len=46) :: DESC
  character(len=32) :: MAP_SOURCE
  real :: XLVL
  integer :: NX
  integer :: NY
  integer :: IPROJ
  real :: STARTLAT
  real :: STARTLON
  real :: DELTALAT
  real :: DELTALON
  real :: DX
  real :: DY
  real :: XLONC
  real :: TRUELAT1
  real :: TRUELAT2
  real :: NLATS
  real :: EARTH_RADIUS = 6367470. * .001
  logical :: IS_WIND_EARTH_REL = .FALSE.


! SLAB is an allocatable array, because we do not necessarily know in 
! advance the size of the array we need to read.
  real, allocatable, dimension(:,:) :: SLAB

  DATALOOP : DO

!====================================================================================!
! READ in your data from the original source - you need to add the reading code here !
!                                                                                    !
! You need to allocate SLAB (this is a 2D array) and place each 2D slab here before  !
! you can write it out to into the intermadiate file format                          !
!                                                                                    !
! Other information you need to know about your data:                                !
!    Time at which data is valid                                                     !
!    Forecast time of the data                                                       !
!    Source of data - you can make something up, it is never used                    !
!    Field name - NOTE THEY NEED TO MATCH THOSE EXPECTED BY METGRID                  !
!    Units of field                                                                  !
!    Description of data                                                             !
!    Level of data - Pa, 200100 Pa is used for surface, and 201300 Pa is used        !
!          for sea-level pressure                                                    !
!    X dimension                                                                     !
!    Y dimension                                                                     !
!    Data projection - only recognize                                                !
!         0:  Cylindrical Equidistant (Lat/lon) projection.                          !
!         1:  Mercator projection.                                                   !
!         3:  Lambert-conformal projection.                                          !
!         4:  Gaussian projection.                                                   !
!         5:  Polar-stereographic projection.                                        !
!    Start location of data - "CENTER", "SWCORNER". "SWCORNER" is typical            !
!    Start lat & long of data                                                        !
!    Lat/Lon increment                                                               !
!    Number of latitudes north of equator (for Gaussian grids)                       !
!    Grid-spacing in x/y                                                             !
!    Center long                                                                     !
!    truelat1/2                                                                      !
!    Has the winds been rotated                                                      !
!====================================================================================!

     write (IUNIT, IOSTAT=IERR) IFV

     ! WRITE the second record, common to all projections:

     write (IUNIT) HDATE, XFCST, MAP_SOURCE, FIELD, UNITS, DESC, XLVL, NX, NY, IPROJ
     print*, HDATE//"  ", XLVL, FIELD

     ! WRITE the third record, which depends on the projection:

     if (IPROJ == 0) then 

        !  This is the Cylindrical Equidistant (lat/lon) projection:
        WRITE (IUNIT) STARTLOC, STARTLAT, STARTLON, DELTALAT, DELTALON, EARTH_RADIUS

     elseif (IPROJ == 1) then 

        ! This is the Mercator projection:
        WRITE (IUNIT) STARTLOC, STARTLAT, STARTLON, DX, DY, TRUELAT1, EARTH_RADIUS

     elseif (IPROJ == 3) then

        ! This is the Lambert Conformal projection:
        WRITE (IUNIT) STARTLOC, STARTLAT, STARTLON, DX, DY, XLONC, TRUELAT1, TRUELAT2, EARTH_RADIUS
        

     elseif (IPROJ == 4) then

        ! Gaussian projection                         
        WRITE (IUNIT) STARTLOC, STARTLAT, STARTLON, NLATS, DELTALON, EARTH_RADIUS
        
     elseif (IPROJ == 5) then

        ! This is the Polar Stereographic projection:
        WRITE (IUNIT) STARTLOC, STARTLAT, STARTLON, DX, DY, XLONC, TRUELAT1, EARTH_RADIUS

     endif

     
     WRITE (IUNIT) IS_WIND_EARTH_REL


     WRITE (IUNIT) slab

     ! Now that we have done all that we want to with SLAB, we need to
     ! deallocate it:

     deallocate(slab)

     ! Loop back to read/write the next field.
  ENDDO DATALOOP

  write(*,'(/,"End of read loop.  Program finished.")')

end program sample_read
