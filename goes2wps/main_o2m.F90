!-----------------------------------------------------
! Yonggang G. Yu
! yuxxx135@umn.edu
! 3-Nov-2022
!-----------------------------------------------------
!
!  driver of the main code
!  
program   main_defect
  use control_para, only : ichart, timevalues, mx_str_L, keywds
  use wps_geom_para
  use goes_R_para
  implicit none
  !
  ! local
  !
  integer :: nnfl=0
  integer :: length_mx, seg_mx, nseg
  integer :: icompute
  logical :: jstatus
  character (len=mx_str_L):: CH100
  integer :: msg
  integer :: i, j, k
  
  length_mx=mx_str_L; seg_mx=mx_str_L
  allocate (keywds(length_mx))

  
  call date_and_time(VALUES=timevalues)
  write (6, '(2x,(a,2x,i4,a,i2,a,i2,2x,i2,a,i2,a,i2,/))')  &
       'Date-time: ', &
       timevalues(1), '-', timevalues(2), '-', timevalues(3), &
       timevalues(5), ':', timevalues(6), ':', timevalues(7)


  open (ichart, file='chart_o2m.in', status='unknown')
  !!open (iucr,   file='d.corr',    status='unknown')

  call scan_begin (ichart, 'Module_0', .true.)
  read (ichart, *) icompute
  !
  !-- read goes_abi_converter module
  call scan_begin (ichart, 'Module_1', .true.)
  call scan_begin (ichart, 'nc_list_file', .false.); backspace(ichart); read(ichart, *) CH100
  call split_string &
       (CH100, '=', length_mx, seg_mx, nseg, keywds, jstatus)

  write(6, '(10a5)') keywds(1:nseg)
  
  backspace(ichart)
  read(ichart, '(a100)') CH100





  
  deallocate(keywds)
end program main_defect
