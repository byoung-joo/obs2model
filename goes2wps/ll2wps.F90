!----------------------------------------------------------------------
! Yonggang G. Yu
! 31-Oct-2022
! Code starting from https://www2.mmm.ucar.edu/wrf/OnLineTutorial/Basics/IM_files/sample.f90
!----------------------------------------------------------------------
!----
!********
!
program write_latlon_to_wps
!   This is a simple program to write data in the WPS intermediate
!   format.  It is included mainly as an aid to understanding said 
!   format.
  use control_para
  use wps_geom_para
  implicit none

! SLAB is an allocatable array, because we do not necessarily know in 
! advance the size of the array we need to read.
  real, allocatable, dimension(:,:) :: SLAB
  integer :: iu, iut   ! iunit, iunit_test
  integer :: it        ! iter count

  ! local
  real, allocatable :: lon(:), lat(:)
  integer :: i, j, k
  read (iunit, nml=ctrl)  
  write(6,  nml=ctrl)
  
  iu=11; iut=6
  open (iu, file=trim(infilename), status='unknown')

  
  it=0 
  DATALOOP : DO
     it=it+1; if (it>2) exit
     call scan_begin (iu, 'HDATE', .true.)
     read(iu, *) HDATE, XFCST, MAP_SOURCE, FIELD, UNITS, DESC, XLVL, NX, NY, IPROJ
     write(iut, *) HDATE, XFCST, MAP_SOURCE, FIELD, UNITS, DESC, XLVL, NX, NY, IPROJ

     stop -1
     
!     read(iu, *)  STARTLOC, STARTLAT, STARTLON, DELTALAT, DELTALON, EARTH_RADIUS
!     write(iut, *)  STARTLOC, STARTLAT, STARTLON, DELTALAT, DELTALON, EARTH_RADIUS
     ! setup lon-lat
     dx=360.d0/real(NX); dy=180.d0/real(NY)
     allocate(lon(NX), lat(NY))
!     allocate(lat(NY))
     do i=0, NX-1
        lon(i)= ( startLon + (0.5d0 + i)*dx )/180.d0 * pi
     enddo
     do j=0, NY-1
        lat(j)= ( startLat + (0.5d0 + j)*dx )/180.d0 * pi
     enddo
     write(6,103) lon, lat


     stop 'nail 1'
     deallocate(lon, lat)
     
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

     write (iuwps, IOSTAT=IERR) IFV

     ! WRITE the second record, common to all projections:

     write (iuwps) HDATE, XFCST, MAP_SOURCE, FIELD, UNITS, DESC, XLVL, NX, NY, IPROJ
     print*, HDATE//"  ", XLVL, FIELD

     ! WRITE the third record, which depends on the projection:

     if (IPROJ == 0) then 

        !  This is the Cylindrical Equidistant (lat/lon) projection:
        WRITE (iuwps) STARTLOC, STARTLAT, STARTLON, DELTALAT, DELTALON, EARTH_RADIUS

     elseif (IPROJ == 1) then 

        ! This is the Mercator projection:
        WRITE (iuwps) STARTLOC, STARTLAT, STARTLON, DX, DY, TRUELAT1, EARTH_RADIUS

     elseif (IPROJ == 3) then

        ! This is the Lambert Conformal projection:
        WRITE (iuwps) STARTLOC, STARTLAT, STARTLON, DX, DY, XLONC, TRUELAT1, TRUELAT2, EARTH_RADIUS
        

     elseif (IPROJ == 4) then

        ! Gaussian projection                         
        WRITE (iuwps) STARTLOC, STARTLAT, STARTLON, NLATS, DELTALON, EARTH_RADIUS
        
     elseif (IPROJ == 5) then

        ! This is the Polar Stereographic projection:
        WRITE (iuwps) STARTLOC, STARTLAT, STARTLON, DX, DY, XLONC, TRUELAT1, EARTH_RADIUS

     endif

     
     WRITE (iuwps) IS_WIND_EARTH_REL


     WRITE (iuwps) slab

     ! Now that we have done all that we want to with SLAB, we need to
     ! deallocate it:

     deallocate(slab)

     ! Loop back to read/write the next field.
  ENDDO DATALOOP

  write(*,'(/,"End of read loop.  Program finished.")')

  include '../myformat.inc' 
end program write_latlon_to_wps
