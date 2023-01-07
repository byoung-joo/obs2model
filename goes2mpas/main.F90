!-----------------------------------------------------
! Yonggang G. Yu
! yuxxx135@umn.edu
! 3-Nov-2022, 2-Dec-2022
!-----------------------------------------------------
!
!  driver of the main code
!  
program   main_o2m
  use kinds, only : sp, dp
  use control_para, only : ichart, timevalues, mx_str_L, keywds, pi
  use wps_geom_para
  use atlas_module, only: atlas_geometry, atlas_indexkdtree
  use mod_goes_abi, only: converter_nml, Goes_ReBroadcast_converter, nml_unit, data_nml
  use netcdf_mod  !BJJ
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
  integer :: ia, ja, ka, ix, i, j
  integer :: ndim_mx, NF_mx, ndim, NF, npts_s   ! _s : satellite
  integer :: nlevel, nip
  type(converter_nml) :: goesR
  
  real(dp), allocatable :: lon(:), lat(:)
  real(sp), allocatable :: lon_s(:), lat_s(:)
  real(sp), allocatable :: field_s(:,:)
  real(dp), allocatable :: lon_s_valid(:), lat_s_valid(:)
  real(dp), allocatable :: field_s_valid(:,:)

  !BJJ TO Read MPAS LAT/LON
  integer :: ncid, nf_status !BJJ
 
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


  !-- WPS GRID for testing
  NXG=101; NYG=101
  dx=360.d0/dble(NXG); dy=180.d0/dble(NYG)
  allocate(lon(0:NXG-1), lat(0:NYG-1))
  do i=0, NXG-1
     lon(i)= ( startLon + (0.5d0 + i)*dx )/180.d0 * pi
  enddo
  do j=0, NYG-1
     lat(j)= ( startLat + (0.5d0 + j)*dy )/180.d0 * pi
  enddo
  !write(6,*) lon
  !write(6,*) lat

  
  !-- read lon / lat / field for satellite     
  !
  ndim_mx= 5424 * 5424       !BJJ Known parameters
  NF_mx= 2                   !BJJ BT13 & CM
  allocate (lon_s(ndim_mx), lat_s(ndim_mx), field_s(ndim_mx, NF_mx))
  lon_s   = -999.
  lat_s   = -999.
  field_s = -999.

  !-- read goes-abi namelist
  ! read namelist
  !
!  open(unit=nml_unit, file='namelist.goes_abi_converter', status='old', form='formatted')
!  read(unit=nml_unit, nml=data_nml, iostat=istatus)
!  write(0,nml=data_nml)
!  if ( istatus /= 0 ) then
!     write(0,*) 'Error reading namelist data_nml'
!     stop
!  end if
  
  !-- note output field_s(ndim, NF)  missing got_lonlat [logical]:  mask ?? 
  !-- 
  call Goes_ReBroadcast_converter (ndim_mx, NF_mx, ndim, NF, lon_s, lat_s, field_s )
  npts_s= ndim   ! pts from satellite

  write(6,*) 'af call Goes_ReBroadcast_converter'

  do ia=ndim_mx/2,ndim_mx/2+100
     write(6, *) 'ia, lon_s(i), lat_s(i)', ia, lon_s(ia), lat_s(ia), field_s(ia,:)
  enddo
  
  !--remove missing array
  ix=0 !count
  do ia=1,ndim_mx
     if(field_s(ia,1).ne.-999.0) then
        ix=ix+1
!     else
!        write(6,*) lon_s(ia), lat_s(ia)
     end if
  enddo
  write(6,*) 'BJJ, ndim_mx, ndim, nvalid=',ndim_mx,5424*5424,ix
  ix=0 !count
  do ia=1,ndim_mx
     if(field_s(ia,2).ne.-999.0) then
        ix=ix+1
!     else
!        write(6,*) lon_s(ia), lat_s(ia)
     end if
  enddo
  write(6,*) 'BJJ, ndim_mx, ndim, nvalid=',ndim_mx,5424*5424,ix
  allocate (lon_s_valid(ix), lat_s_valid(ix), field_s_valid(ix, NF_mx))
  !lon_s_valid(:)=pack(lon_s,field_s(:,1).eq.-999.0)
  !lat_s_valid(:)=pack(lon_s,field_s(:,1).eq.-999.0)
  !field_s_valid(:,1)=pack(lon_s,field_s(:,1).eq.-999.0)
  !field_s_valid(:,2)=pack(lon_s,field_s(:,1).eq.-999.0)
  ix=0 !count
  do ia=1,ndim_mx
     if (field_s(ia,2).eq.-999.0) then
        cycle
     else
       ix=ix+1
       lon_s_valid(ix)=lon_s(ia)
       lat_s_valid(ix)=lat_s(ia)
       field_s_valid(ix,:)=field_s(ia,:)
     end if
  enddo 

  write(6,*) 'BJJ, lat=',lat_s_valid(1:30)
  write(6,*) 'BJJ, tb=',field_s_valid(1:30,1)
  write(6,*) 'BJJ, cm=',field_s_valid(1:30,2)

!  stop 'nail x'
 

!--BJJ Read MPAS LAT/LON
!  nf_status = nf_OPEN(trim(fname), nf_NOWRITE, ncid)
  if ( nf_status .ne. 0 ) then
!      write(0,*) 'ERROR reading '//trim(fname)
      STOP
  end if
!
!  nf_status(1) = nf_INQ_DIMID(ncid, 'x', dimid)
!  nf_status(2) = nf_INQ_DIMLEN(ncid, dimid, nx)
!  nf_status(3) = nf_INQ_DIMID(ncid, 'y', dimid)
!  nf_status(4) = nf_INQ_DIMLEN(ncid, dimid, ny)
!  if ( any(nf_status /= 0) ) then
!      write(uop,*) 'Error reading dimensions'
!      stop
!   end if
!
!   istart(1) = 1
!   icount(1) = nx
!   allocate(itmp_short_1d(nx))
!   nf_status = nf_INQ_VARID(ncid, 'x', varid)
!   nf_status = nf_GET_VARA_INT2(ncid, varid, istart(1:1), icount(1:1), itmp_short_1d(:))
!   nf_status = nf_GET_ATT_REAL(ncid, varid, 'scale_factor', scalef)
!   nf_status = nf_GET_ATT_REAL(ncid, varid, 'add_offset', offset)
!   allocate(x(nx))
!   do i = 1, nx
!      x(i) = offset + itmp_short_1d(i) * scalef
!   end do
!   deallocate(itmp_short_1d)
!---------------------------------------------- 

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
  call kd%build(npts_s, real(lon_s,dp), real(lat_s,dp))
  
  ia=0
  do i=0, NXG-1
     do j=0, NYG-1
        lon(i)= ( startLon + (0.5d0 + i)*dx )/180.d0 * pi
        lat(j)= ( startLat + (0.5d0 + j)*dy )/180.d0 * pi
        ia=ia+1
        ! get nn index
        call kd%closestPoints(lon(i), lat(j), nn, interp_indx(:,ia))

        write(6,*) 'interp_indx(1:nn,ia)', interp_indx(1:nn,ia)
        write(6,*) 'reg. lon, lat', lon(i), lat(j)
        do ka=1, nn
           ix=interp_indx(ka,ia)
           write(6,*) 'ith nn, ix,  lon, lat', ka, ix, lon_s(ix), lat_s(ix)
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
  write(6,*) 'icompute', icompute
  write(6,*) 'istatus L47', istatus
  write(6, *) 'CH100', CH100  
  write(6, *) 'nseg=', nseg
  write(6, '(10a)') trim(keywds(1))
  write(6, '(10a)') trim(keywds(nseg))
  
  
  deallocate(keywds)
  deallocate(lon, lat, lon_s, lat_s)
end program main_o2m
