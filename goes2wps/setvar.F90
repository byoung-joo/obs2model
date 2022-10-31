!-----------------------------------------------------
! Yonggang G. Yu 
! 31-Oct-2022
!-----------------------------------------------------


!------
!************************
!------  var_module.f90
!************************
!

module control_para
!
!
!==section 1
!  control variables
!
  implicit none
  SAVE
  integer, parameter :: mx_str_L=30
  character (len=mx_str_L) :: calculation
  character (len=mx_str_L), allocatable :: keywds(:)
  !
  !
  integer, parameter :: stdout   = 6       ! output unit
  integer, parameter :: iunit    = 5       ! readin unit
  integer, parameter :: ounit    = 11       ! readin unit
  integer, parameter :: flunit   = 21      ! files unit for inp_ph_flname
  integer, parameter :: iucmp    = 22      !            for composition reading
  integer, parameter :: iprt0    = 1000    ! 
  integer, parameter :: nfile_max = 30

  character (len=50) :: infilename

  namelist /ctrl/  infilename

end module control_para

module wps_geom_para
!
!==section 2
!  Computational geometry 
!
  implicit none
  SAVE
  integer :: IFV=5
  character(len=24) :: HDATE
  real :: XFCST
  character(len=8)  :: STARTLOC
  character(len=9)  :: FIELD
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
  
  !

  !
  !-- for strings
  !
  integer          :: seg_mx   = 10
  integer          :: nseg

end module wps_geom_para

