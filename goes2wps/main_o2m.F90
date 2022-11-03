!-----------------------------------------------------
! Yonggang G. Yu
! yuxxx135@umn.edu
! 3-Nov-2022
!-----------------------------------------------------
!
!  driver of the main code
!  
program   main_defect
  use control_para, only : ichart, timevalues, mx_str_L, keywds, pi
  use wps_geom_para
  use goes_R_para
  use atlas_module, only: atlas_geometry, atlas_indexkdtree
  implicit none
  !
  ! local
  !
  integer :: nnfl=0
  integer :: length_mx, seg_mx, nseg
  integer :: icompute
  integer :: istatus
  logical :: jstatus
  character (len=mx_str_L):: CH100, CHX
  integer :: msg
  integer :: i, j, k
  type(converter_nml) :: goesR

  real, allocatable :: lon(:), lat(:)
  real, allocatable :: lon_s(:), lat_s(:)

  type(atlas_indexkdtree) :: kd
  type(atlas_geometry) :: ageometry
  
  length_mx=mx_str_L; seg_mx=mx_str_L
  allocate (keywds(length_mx))
  
  
  call date_and_time(VALUES=timevalues)
  write (6, '(2x,(a,2x,i4,a,i2,a,i2,2x,i2,a,i2,a,i2,/))')  &
       'Date-time: ', &
       timevalues(1), '-', timevalues(2), '-', timevalues(3), &
       timevalues(5), ':', timevalues(6), ':', timevalues(7)


  open (ichart, file='chart_o2m.in', status='unknown')
  !!open (iucr,   file='d.corr',    status='unknown')

  call scan_begin (ichart, 'Module_0', .true., istatus)
  read (ichart, *) icompute


  !-- read goes_abi_converter module
  !   issue/bug:  used single quotation , code fails in double quote
  !
  goesR%nc_list_file=''
  goesR%data_dir=''
  goesR%data_id=''
  goesR%sat_id=''
  goesR%n_subsample=0
  !
  call scan_begin (ichart, 'Module_1', .true., istatus)
  call scan_begin (ichart, 'nc_list_file', .true., istatus); backspace(ichart); read(ichart, '(a100)') CH100
  call split_string &
       (CH100, "'", length_mx, seg_mx, nseg, keywds, jstatus)
  call remove_quote_in_string(keywds(2),CHX,length_mx)
  goesR%nc_list_file = trim(CHX)
  !
  call scan_begin (ichart, 'data_dir', .true., istatus); backspace(ichart); read(ichart, '(a100)') CH100
  call split_string &
       (CH100, "'", length_mx, seg_mx, nseg, keywds, jstatus)
  call remove_quote_in_string(keywds(2),CHX,length_mx)
  goesR%data_dir = trim(CHX)
  !
  call scan_begin (ichart, 'data_id', .true., istatus); backspace(ichart); read(ichart, '(a100)') CH100
  call split_string &
       (CH100, "'", length_mx, seg_mx, nseg, keywds, jstatus)
  call remove_quote_in_string(keywds(2),CHX,length_mx)
  goesR%data_id = trim(CHX)
  !
  call scan_begin (ichart, 'sat_id', .true., istatus); backspace(ichart); read(ichart, '(a100)') CH100
  call split_string &
       (CH100, "'", length_mx, seg_mx, nseg, keywds, jstatus)
  call remove_quote_in_string(keywds(2),CHX,length_mx)
  goesR%sat_id = trim(CHX)
  !
  call scan_begin (ichart, 'n_subsample', .true., istatus); backspace(ichart); read(ichart, '(a100)') CH100
  call split_string &
       (CH100, '=', length_mx, seg_mx, nseg, keywds, jstatus)
  read(keywds(2), *) goesR%n_subsample 


  
  
  !-- read WPS lat-lon setup
  !   issue/bug:  
  !  
  call scan_begin (ichart, 'HDATE', .true., istatus)
  read(ichart, *) HDATE, XFCST, MAP_SOURCE, FIELD, UNITS, DESC, XLVL, NX, NY, IPROJ
  write(6, *) HDATE, XFCST, MAP_SOURCE, FIELD, UNITS, DESC, XLVL, NX, NY, IPROJ
  call scan_begin (ichart, 'STARTLOC', .false., istatus)
  read(ichart, *)  STARTLOC, STARTLAT, STARTLON, DELTALAT, DELTALON, EARTH_RADIUS
  write(6, *)  STARTLOC, STARTLAT, STARTLON, DELTALAT, DELTALON, EARTH_RADIUS



  dx=360.d0/real(NX); dy=180.d0/real(NY)
  allocate(lon(0:NX-1), lat(0:NY-1))
  do i=0, NX-1
     lon(i)= ( startLon + (0.5d0 + i)*dx )/180.d0 * pi
  enddo
  do j=0, NY-1
     lat(j)= ( startLat + (0.5d0 + j)*dy )/180.d0 * pi
  enddo
  write(6,103) lon
  write(6,103) lat

     
  !   ! setup lon-lat satellite     
  !   call read_obs_output_coords_fields (filename, 
     
     stop 'nail 1'
     deallocate(lon, lat)
     stop -1


  


  
  write(6, *) goesR

  !
!
!
!   character(len=256)         :: nc_list_file  !  contains a list of netcdf files to process
!   character(len=256)         :: data_dir
!   character(len=18)          :: data_id
!   character(len=3)           :: sat_id
!   integer(kind=i_long)       :: n_subsample
!   nc_list_file
!
  
  !-- print
  write(6,121) 'icompute', icompute
  write(6,121) 'istatus L47', istatus
  write(6, 102) 'CH100', CH100  
  write(6, 121) 'nseg=', nseg
  write(6, '(10a)') trim(keywds(1))
  write(6, '(10a)') trim(keywds(nseg))
  




  


  
  deallocate(keywds)
  include '../myformat.inc'
end program main_defect

