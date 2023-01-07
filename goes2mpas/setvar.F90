!-----------------------------------------------------
! Yonggang G. Yu
! yuxxx135@umn.edu
! 31-Oct-2022
!-----------------------------------------------------

module kinds
implicit none
save
integer, parameter, public :: sp = selected_real_kind(6)    ! REAL*4
integer, parameter, public :: dp = selected_real_kind(15)   ! REAL*8
#ifdef DOUBLEPRECISION
integer, parameter, public :: rt = dp
#else
integer, parameter, public :: rt = dp    ! default
#endif
 
end module kinds


module control_para
  implicit none
  SAVE
  real*8, parameter :: pi       = 3.1415926535897932384626433832795d0
  integer            :: ierr
  integer            :: timevalues(8)      ! 
end module control_para