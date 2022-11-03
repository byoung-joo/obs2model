!-----------------------------------------------------
! Yonggang G. Yu
! yuxxx135@umn.edu
! 3-Nov-2022
!-----------------------------------------------------
!
!  driver of the main code
!  
program   main_defect
  use contrl_para, only : ichart, timevalues
  use wps_geom_para
  use goes_R_para
  implicit none
  !
  ! local
  !
  integer :: nnfl=0
  integer :: length_mx=100
  integer :: iextractDFT, icompute, i, j, k
  logical :: jstatus
  character (len=length_mx):: CH100
  integer :: msg

  length_mx=200
  call date_and_time(VALUES=timevalues)
  write (6, '(2x,(a,2x,i4,a,i2,a,i2,2x,i2,a,i2,a,i2,/))')  &
       'Date-time: ', &
       timevalues(1), '-', timevalues(2), '-', timevalues(3), &
       timevalues(5), ':', timevalues(6), ':', timevalues(7)


  open (ichart, file='chart_o2m.in', status='unknown')
  open (iucr,   file='d.corr',    status='unknown')

  call scan_begin (ichart, 'Module_0', .true.)
  read (ichart, *) icompute
  !
  !-- read goes_abi_converter module
  call scan_begin (ichart, 'Module_1', .true.)
  call scan_begin (ichart, 'nc_list_file', .false.); backspace(ichart); read(ichart, *) CH100
!  call split_string &
!       (name_cmpd_conv(icomp), '_', length_mx, seg_mx, nseg, keywds, jstatus)
 
  backspace(ichart)
  read(ichart, '(a100)') CH100

end program main_defect
