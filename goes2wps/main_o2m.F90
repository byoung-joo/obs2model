!-----------------------------------------------------
! Yonggang G. Yu
! yuxxx135@umn.edu
! 3-Nov-2022, 2-Dec-2022
!-----------------------------------------------------
!
!  driver of the main code
!  
program   main_o2m
  use kinds, only : dp
  use control_para, only : ichart, timevalues, mx_str_L, keywds, pi
  use wps_geom_para
  use goes_R_para
  use atlas_module, only: atlas_geometry, atlas_indexkdtree
  use mod_goes_abi
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
  integer :: ia, ja, ka, ix
  integer :: ndim_mx, NF_mx, ndim, NF, npts_s   ! _s : satellite
  integer :: nlevel, nip
  type(converter_nml) :: goesR
  
  real(dp), allocatable :: lon(:), lat(:)
  real(dp), allocatable :: lon_s(:), lat_s(:)
  real(dp), allocatable :: field_s(:,:)
  
  type(atlas_indexkdtree) :: kd
  type(atlas_geometry) :: ageometry

  integer :: nn
  integer, allocatable :: interp_indx(:,:)
  
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
  read(ichart, *) HDATE, XFCST, MAP_SOURCE, FIELD, UNITS, DESC, XLVL, NXG, NYG, IPROJ
  write(6, *) HDATE, XFCST, MAP_SOURCE, FIELD, UNITS, DESC, XLVL, NXG, NYG, IPROJ
  call scan_begin (ichart, 'STARTLOC', .false., istatus)
  read(ichart, *)  STARTLOC, STARTLAT, STARTLON, DELTALAT, DELTALON, EARTH_RADIUS
  write(6, *)  STARTLOC, STARTLAT, STARTLON, DELTALAT, DELTALON, EARTH_RADIUS


  dx=360.d0/dble(NXG); dy=180.d0/dble(NYG)
  allocate(lon(0:NXG-1), lat(0:NYG-1))
  do i=0, NXG-1
     lon(i)= ( startLon + (0.5d0 + i)*dx )/180.d0 * pi
  enddo
  do j=0, NYG-1
     lat(j)= ( startLat + (0.5d0 + j)*dy )/180.d0 * pi
  enddo
  write(6,103) lon
  write(6,103) lat

  
  !-- read lon / lat / field for satellite     
  !
  nlevel= 11
  nip= 10*4**nlevel + 2
  ndim_mx= nip
  NF_mx= 300
  allocate (lon_s(ndim_mx), lat_s(ndim_mx), field_s(ndim_mx, NF_mx))

  
  !-- missing got_lonlat:  mask ??
  !-- 
  call Goes_ReBroadcast_converter ( goesR, ndim_mx, NF_mx, ndim, NF, lon_s, lat_s, field_s )
  npts_s= ndim   ! pts from satellite

  write(6,101) 'af call Goes_ReBroadcast_converter'

  do ia=1, 4
     write(6, 141) 'ia, lon_s(i), lat_s(i)', ia, lon_s(ia), lat_s(ia)
  enddo
  
  write(6,101) 'read sat ABI failed, lon, lat wrong!  '
  write(6,101) 'possible reasons'
  write(6,101) '1. kind_real  SP vs DP is set wrong in goes_abi_mod.F90'
  write(6,101) '2. goes_abi_mod.F90 reading sat. data problem'

  stop 'nail x'
  

  !-- interpolation
  !   1. get index for 3-point interpolation
  !   2. get wj; weighted sum
  !   input: obs grid
  !   target: model grid
  !
  ! buid kd-tree, find nn index [obs 2 model]
  nn=3
  allocate (interp_indx(nn, NXG*NYG))
  ageometry = atlas_geometry("UnitSphere")
  kd = atlas_indexkdtree(ageometry)
  call kd%reserve(npts_s)
  call kd%build(npts_s, lon_s, lat_s)
  
  ia=0
  do i=0, NXG-1
     do j=0, NYG-1
        lon(i)= ( startLon + (0.5d0 + i)*dx )/180.d0 * pi
        lat(j)= ( startLat + (0.5d0 + j)*dy )/180.d0 * pi
        ia=ia+1
        ! get nn index
        call kd%closestPoints(lon(i), lat(j), nn, interp_indx(:,ia))

        write(6,121) 'interp_indx(1:nn,ia)', interp_indx(1:nn,ia)
        write(6,101) 'reg. lon, lat', lon(i), lat(j)
        do ka=1, nn
           ix=interp_indx(ka,ia)
           write(6,146) 'ith nn, ix,  lon, lat', ka, ix, lon_s(ix), lat_s(ix)
        enddo
        stop 'nail 1'

        !
        !  sample code from JEDI
        !
        !

!       case ('barycent')
!       allocate(bw(self%nn))
!       do n = 1,ngrid_out
!          !Barycentric weights formula
!          bw(:) = 0.0_kind_real
!          do jj = 1, nn
!             wprod = 1.0_kind_real
!             index1 = self%interp_i(jj,n)
!             do kk = 1, nn
!                if (jj.ne.kk) then
!                   index2 = self%interp_i(kk,n)
!                   dist = ageometry%distance(lons_in_glo(index1),lats_in_glo(index1),&
!                        lons_in_glo(index2),lats_in_glo(index2))
!                   wprod = wprod * max(dist, 1e-10)
!                endif
!             enddo
!             bw(jj) = 1.0_kind_real / wprod
!          enddo
!          !
!          !Barycentric weights
!          self%interp_w(:,n) = 0.0_kind_real
!          if (minval(nn_dist(:,n)) < 1e-10) then
!             ! special case if very close to one grid point
!             jj = minloc(nn_dist(:,n),dim=1)
!             self%interp_w(jj,n) = 1.0_kind_real
!          else
!             !otherwise continue with the normal algorithm
!             bsw = 0.0_kind_real
!             do jj = 1,nn
!                bsw = bsw + (bw(jj) / nn_dist(jj,n))
!             enddo
!             do jj = 1,nn
!                self%interp_w(jj,n) = ( bw(jj) / nn_dist(jj,n) ) / bsw
!             enddo
!          end if
!       enddo
!       deallocate(bw)
!       case default        

     enddo
  enddo


  
  deallocate( interp_indx )
  deallocate( lon, lat )
  !
  
  stop -1
  
  write(6, *) goesR

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
  deallocate(lon, lat, lon_s, lat_s)
  include '../myformat.inc'
end program main_o2m
