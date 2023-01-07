!-----------------------------------------------------
! Yonggang G. Yu
! yuxxx135@umn.edu
! 3-Nov-2022, 2-Dec-2022
!-----------------------------------------------------
!
!  driver of the main code
!  
program   main
  use kinds, only : sp, dp
  use control_para, only : timevalues
  use atlas_module, only: atlas_geometry, atlas_indexkdtree
  use mod_goes_abi, only: converter_nml, Goes_ReBroadcast_converter, nml_unit, data_nml
  use mod_read_mpas, only: read_mpas_latlon
  implicit none
  !
  ! local
  !
  integer :: nnfl=0
  integer :: icompute
  integer :: istatus
  logical :: jstatus
  integer :: msg
  integer :: ia, ja, ka, ix, i, j
  integer :: ndim_mx, NF_mx, ndim, NF, npts_s   ! _s : satellite
  integer :: nlevel, nip
  type(converter_nml) :: goesR
  
  real(sp), allocatable :: lon_s(:), lat_s(:)
  real(sp), allocatable :: field_s(:,:)
  real(sp), allocatable :: lon_mpas(:), lat_mpas(:)
  real(dp), allocatable :: lon_s_valid(:), lat_s_valid(:)
  real(dp), allocatable :: field_s_valid(:,:)
  real(dp), allocatable :: lon_mpas_dp(:), lat_mpas_dp(:)

  !BJJ TO Read MPAS LAT/LON
  integer :: ncid, nf_status !BJJ
  integer :: nC, iC
 
  type(atlas_indexkdtree) :: kd
  type(atlas_geometry) :: ageometry

  integer :: nn
  integer, allocatable :: interp_indx(:,:)
  
  call date_and_time(VALUES=timevalues)
  write (6, '(2x,(a,2x,i4,a,i2,a,i2,2x,i2,a,i2,a,i2,/))')  &
       'Date-time: ', &
       timevalues(1), '-', timevalues(2), '-', timevalues(3), &
       timevalues(5), ':', timevalues(6), ':', timevalues(7)


  !-- read lon / lat / field for satellite     
  !
  ndim_mx= 5424 * 5424       !BJJ Known parameters
  NF_mx= 2                   !BJJ BT13 & CM
  allocate (lon_s(ndim_mx), lat_s(ndim_mx), field_s(ndim_mx, NF_mx))
  lon_s   = -999.
  lat_s   = -999.
  field_s = -999.

  !-- note output field_s(ndim, NF)  missing got_lonlat [logical]:  mask ?? 
  !-- 
  call Goes_ReBroadcast_converter (ndim_mx, NF_mx, ndim, NF, lon_s, lat_s, field_s )
  npts_s= ndim   ! pts from satellite
write(6,*) 'BJJ min/max of lon_s = ',minval(lon_s),maxval(lon_s)
write(6,*) 'BJJ min/max of lat_s = ',minval(lat_s),maxval(lat_s)

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
  npts_s=ix !BJJ used for kd-tree
  write(6,*) 'BJJ, ndim_mx, ndim, nvalid=',ndim_mx,5424*5424,ix
  allocate (lon_s_valid(ix), lat_s_valid(ix), field_s_valid(ix, NF_mx))
  !lon_s_valid(:)=pack(lon_s,field_s(:,1).eq.-999.0)
  !lat_s_valid(:)=pack(lon_s,field_s(:,1).eq.-999.0)
  !field_s_valid(:,1)=pack(lon_s,field_s(:,1).eq.-999.0)
  !field_s_valid(:,2)=pack(lon_s,field_s(:,1).eq.-999.0)
  ix=0 !count
  do ia=1,ndim
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
   call read_mpas_latlon (nC, lon_mpas, lat_mpas)
  write(6,*) 'BJJ, lon_mpas(1:10)=',lon_mpas(1:10)
  write(6,*) 'BJJ, lat_mpas(1:10)=',lat_mpas(1:10)
  lon_mpas_dp=lon_mpas
  lat_mpas_dp=lat_mpas
  do iC=1,nC
    if(lon_mpas_dp(iC).gt.3.141592d0) lon_mpas_dp(iC)=lon_mpas_dp(iC)-2.d0*3.141592d0
  end do

!-----check BJJ
write(6,*) 'BJJ min/max of lon_s [deg] =',minval(lon_s),maxval(lon_s)
write(6,*) 'BJJ min/max of lat_s [deg] =',minval(lat_s),maxval(lat_s)
write(6,*) 'BJJ min/max of lon_s_valid [deg] =',minval(lon_s_valid),maxval(lon_s_valid)
write(6,*) 'BJJ min/max of lat_s_valid [deg] =',minval(lat_s_valid),maxval(lat_s_valid)
write(6,*) 'BJJ min/max of lon_mpas_dp [deg] =',minval(lon_mpas_dp)*180./3.1415,maxval(lon_mpas_dp)*180./3.1415
write(6,*) 'BJJ min/max of lat_mpas_dp [deg] =',minval(lat_mpas_dp)*180./3.1415,maxval(lat_mpas_dp)*180./3.1415
  !-- interpolation
  !   1. get index for 3-point interpolation
  !   2. get wj; weighted sum
  !   input: obs grid
  !   target: model grid
  !
  ! buid kd-tree, find nn index [obs 2 model]
  nn=3
  allocate (interp_indx(nn, nC))   ! nC <--> npts_s
  ageometry = atlas_geometry("UnitSphere")
  kd = atlas_indexkdtree(ageometry)
  call kd%reserve(npts_s)
  call date_and_time(VALUES=timevalues)
  write (6, '(2x,(a,2x,i4,a,i2,a,i2,2x,i2,a,i2,a,i2,/))')  &
       'kd%build start', &
       timevalues(1), '-', timevalues(2), '-', timevalues(3), &
       timevalues(5), ':', timevalues(6), ':', timevalues(7)
  !call kd%build(npts_s, real(lon_s,dp), real(lat_s,dp))  !BJJ this includes the missing points
  call kd%build(npts_s, lon_s_valid/180.d0*3.1415926d0, lat_s_valid/180.d0*3.1415926d0)
  !call kd%build(nC, lon_mpas_dp, lat_mpas_dp)
  call date_and_time(VALUES=timevalues)
  write (6, '(2x,(a,2x,i4,a,i2,a,i2,2x,i2,a,i2,a,i2,/))')  &
       'kd%build done', &
       timevalues(1), '-', timevalues(2), '-', timevalues(3), &
       timevalues(5), ':', timevalues(6), ':', timevalues(7)
  
  ia=0
  !do iC=0, npts_s-1
  do iC=1, nC
        ! get nn index
!/180.d0*3.1415926d0
        call kd%closestPoints(lon_mpas_dp(iC), lat_mpas_dp(iC), &
                              nn, interp_indx(:,iC))

    if(mod(iC,int(nC/100)).eq.0) then
  call date_and_time(VALUES=timevalues)
  write (6, '(2x,(a,2x,i4,a,i2,a,i2,2x,i2,a,i2,a,i2,/))')  &
       'kd%closestPoints ING', &
       timevalues(1), '-', timevalues(2), '-', timevalues(3), &
       timevalues(5), ':', timevalues(6), ':', timevalues(7)
        write(6,*) 'interp_indx(1:nn,ia)', interp_indx(1:nn,iC)
        write(6,*) 'MPAS : lon, lat = ', &
                      lon_mpas_dp(iC)*180./3.1415, &
                      lat_mpas_dp(iC)*180./3.1415
        do ka=1, nn
           ix=interp_indx(ka,iC)
           write(6,*) 'ABI :ith nn, ix,  lon, lat', ka, ix, &
                    lon_s_valid(ix), &
                    lat_s_valid(ix)
        enddo
     end if
   end do
  call date_and_time(VALUES=timevalues)
  write (6, '(2x,(a,2x,i4,a,i2,a,i2,2x,i2,a,i2,a,i2,/))')  &
       'kd%closestPoints done', &
       timevalues(1), '-', timevalues(2), '-', timevalues(3), &
       timevalues(5), ':', timevalues(6), ':', timevalues(7)


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

!     enddo
!  enddo


  
  deallocate( interp_indx )
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
  
  deallocate(lon_s, lat_s)
end program main
