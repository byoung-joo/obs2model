!-----------------------------------------------------
! Yonggang G. Yu
! yuxxx135@umn.edu
! 31-Oct-2022
!-----------------------------------------------------

module control_para
  implicit none
  SAVE
  real*8, parameter :: pi       = 3.1415926535897932384626433832795d0
  integer, parameter  :: i_long   = selected_int_kind(8)   ! long integer
  integer :: i_kind=i_long
  integer, parameter :: mx_str_L=100
  character (len=mx_str_L) :: calculation
  character (len=mx_str_L), allocatable :: keywds(:)
  !
  !
  integer, parameter :: stdout   = 6       ! output unit
  integer, parameter :: iunit    = 5       ! readin unit
  integer, parameter :: ichart   = 11      ! 
  integer, parameter :: iuwps    = 23      ! wps binary
  integer, parameter :: nfile_max = 30
  integer            :: ierr
  integer            :: timevalues(8)      ! 
end module control_para


module wps_geom_para
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
end module wps_geom_para


module goes_R_para
  use control_para, only : i_kind
  type converter_nml
     character(len=256)         :: nc_list_file  !  contains a list of netcdf files to process
     character(len=256)         :: data_dir
     character(len=18)          :: data_id
     character(len=3)           :: sat_id
     integer(i_kind)            :: n_subsample
  end type converter_nml
end module goes_R_para
