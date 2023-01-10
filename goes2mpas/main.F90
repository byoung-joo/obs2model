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
  use control_para, only : tval, pii, rad2deg, deg2rad
  use atlas_module, only: atlas_geometry, atlas_indexkdtree
  use mod_goes_abi, only: converter_nml, Goes_ReBroadcast_converter, nml_unit, data_nml
  use mod_read_mpas, only: read_mpas_latlon, write_to_mpas
  implicit none
  !
  ! local
  !
  integer :: nnfl=0
  integer :: icompute
  integer :: istatus
  logical :: jstatus
  integer :: msg
  integer :: i, j, icnt
  integer :: ndim_mx, NF_mx, ndim, NF
  integer :: nlevel, nip

  integer :: nS, iS  ! nS: total number of satellite grid
  real(sp), allocatable :: lon_s(:), lat_s(:)    ! direct read from file
  real(sp), allocatable :: field_s(:,:)          !
  integer :: nS_valid ! only contains # of valid points, removing undefined and missing points.
  real(dp), allocatable :: lon_s_valid(:), lat_s_valid(:)
  real(dp), allocatable :: field_s_valid(:,:)

  !BJJ TO Read MPAS LAT/LON
  integer :: nC, iC  ! nC: total number of MPAS grid
  real(sp), allocatable :: lon_mpas(:), lat_mpas(:)       ! to read. depend on the NetCDF file.
  real(dp), allocatable :: lon_mpas_dp(:), lat_mpas_dp(:) ! for kd-tree
 
  !kd-tree
  type(atlas_indexkdtree) :: kd
  type(atlas_geometry) :: ageometry
  integer :: nn, ix   ! nn= number of nearest point, ix= temporary indice for interp_indx
  integer, allocatable :: interp_indx(:,:)
  !BJJ distance
  real(dp) :: dist, dist_min
  integer :: idx_min
  !BJJ Count # of matching for a given MPAS cell
  integer, allocatable :: cnt_match(:)
  real(dp), allocatable :: field_dist(:,:,:)
  integer :: max_pair

  
  777 format(2x,(a,2x,i4.4,a,i2.2,a,i2.2,2x,i2.2,a,i2.2,a,i2.2,/))
  call date_and_time(VALUES=tval)
  write (6, 777) 'Date-time: ',tval(1),'-',tval(2),'-',tval(3),tval(5),':',tval(6),':',tval(7)

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
  nS= ndim   ! pts from satellite
  call date_and_time(VALUES=tval)
  write (6, 777) 'GOES READ DONE: ',tval(1),'-',tval(2),'-',tval(3),tval(5),':',tval(6),':',tval(7)

  !--remove missing array
  icnt=0 !count
  do iS=1,nS
     if(field_s(iS,1).ne.-999.0) then
        icnt=icnt+1
     end if
  enddo
  write(6,*) 'BJJ, ndim_mx, ndim, nvalid of R13 =',ndim_mx,5424*5424,icnt
  icnt=0 !count
  do iS=1,nS
     if(field_s(iS,2).ne.-999.0) then
        icnt=icnt+1
     end if
  enddo
  write(6,*) 'BJJ, ndim_mx, ndim, nvalid of CM =',ndim_mx,5424*5424,icnt
  nS_valid=icnt !BJJ used for kd-tree
  !-- continue
  allocate (lon_s_valid(nS_valid))
  allocate (lat_s_valid(nS_valid))
  allocate (field_s_valid(nS_valid, NF_mx))
  icnt=0 ! used for indice for valid points
  do iS=1,nS
     if (field_s(iS,2).eq.-999.0) then  !BJJ use CM for masking
        cycle
     else
       icnt=icnt+1
       lon_s_valid(icnt)=lon_s(iS)
       lat_s_valid(icnt)=lat_s(iS)
       field_s_valid(icnt,:)=field_s(iS,:)
     end if
  enddo 

  !--BJJ Read MPAS LAT/LON
  call read_mpas_latlon (nC, lon_mpas, lat_mpas)  ! unit [radian], single precision
  lon_mpas_dp=lon_mpas ! pass double precision for kd-tree
  lat_mpas_dp=lat_mpas
  do iC=1,nC
    ! range [0,360] --> [-180, 180]
    if(lon_mpas_dp(iC).gt.pii) lon_mpas_dp(iC)=lon_mpas_dp(iC)-2.d0*pii
  end do

!-----check BJJ
!write(6,*) 'BJJ min/max of lon_s [deg] =',minval(lon_s),maxval(lon_s)
!write(6,*) 'BJJ min/max of lat_s [deg] =',minval(lat_s),maxval(lat_s)
!write(6,*) 'BJJ min/max of lon_s_valid [deg] =',minval(lon_s_valid),maxval(lon_s_valid)
!write(6,*) 'BJJ min/max of lat_s_valid [deg] =',minval(lat_s_valid),maxval(lat_s_valid)
!write(6,*) 'BJJ min/max of lon_mpas_dp [deg] =',minval(lon_mpas_dp)*rad2deg,maxval(lon_mpas_dp)*rad2deg
!write(6,*) 'BJJ min/max of lat_mpas_dp [deg] =',minval(lat_mpas_dp)*rad2deg,maxval(lat_mpas_dp)*rad2deg

  !-- interpolation
  !   1. get index for 3-point interpolation
  !   2. get wj; weighted sum
  !   input: obs grid
  !   target: model grid
  !
  ! buid kd-tree, find nn index [obs 2 model]
  nn=1  ! number of nearest points
  allocate (interp_indx(nn, nS_valid))   ! nC <--> nS_valid
  ageometry = atlas_geometry("UnitSphere")
  kd = atlas_indexkdtree(ageometry)
  call kd%reserve(nC)
  call date_and_time(VALUES=tval)
  write (6, 777) 'kd%build start: ',tval(1),'-',tval(2),'-',tval(3),tval(5),':',tval(6),':',tval(7)
  call kd%build(nC, lon_mpas_dp, lat_mpas_dp)
  call date_and_time(VALUES=tval)
  write (6, 777) 'kd%build done: ',tval(1),'-',tval(2),'-',tval(3),tval(5),':',tval(6),':',tval(7)

  allocate(cnt_match(nC)) ! count the pairs for a given MPAS point.
  cnt_match(:)=0 !init
  
  do iS=1, nS_valid
    ! get nn index
    call kd%closestPoints(lon_s_valid(iS)*deg2rad, lat_s_valid(iS)*deg2rad, &  !need a unit of [radian]
                          nn, interp_indx(:,iS))

    !print for quick verif.
    if(mod(iS,int(nS_valid/10)).eq.0) then
      write(6,*) 'iS, interp_indx(1:nn,iS)', iS, interp_indx(1:nn,iS)
      write(6,*) 'ABI : lon, lat = ', lon_s_valid(iS), lat_s_valid(iS)
      do i=1, nn
         ix=interp_indx(i,iS)
         write(6,*) 'MPAS :ith nn, ix,  lon, lat', i, ix, &
                    lon_mpas_dp(ix)*rad2deg, lat_mpas_dp(ix)*rad2deg
      enddo
    end if

    !count # of matching !BJJ ONLY work nn=1 ::NOTE::
    ix=interp_indx(1,iS)
    cnt_match(ix)=cnt_match(ix)+1
!Fortran runtime error: Index '0' of dimension 1 of array 'cnt_match' below lower bound of 1
  end do

  call date_and_time(VALUES=tval)
  write (6, 777) 'kd%closestPoints done',tval(1),'-',tval(2),'-',tval(3),tval(5),':',tval(6),':',tval(7)

  ! BJJ: We can write out "interp_indx" as a file.

  ! check the min/max of pairs
  write(6,*) "BJJ MIN/MAX of pairs = ",minval(cnt_match),maxval(cnt_match)
  max_pair=maxval(cnt_match)


  ! distribute the matched pairs (re-organize)
  allocate(field_dist(max_pair,nF_mx+2,nC))  ! 1:nF_mx=field, nF_mx+1=lon, nF_mx+2=lat
  field_dist(:,:,:)=-999.0 !init
  cnt_match(:)=0 !init

  do iS= 1, nS_valid
    ix=interp_indx(1,iS)
    cnt_match(ix)=cnt_match(ix)+1
    field_dist(cnt_match(ix),1:nF_mx,ix)=field_s_valid(iS,1:nF_mx)
    field_dist(cnt_match(ix),nF_mx+1,ix)=lon_s_valid(iS)*deg2rad  !pass as [radian] to make the next step easier.
    field_dist(cnt_match(ix),nF_mx+2,ix)=lat_s_valid(iS)*deg2rad
    !BJJ as write test
  enddo
  call date_and_time(VALUES=tval)
  write (6, 777) 're-organize data done',tval(1),'-',tval(2),'-',tval(3),tval(5),':',tval(6),':',tval(7)

  !-- THIS IS QUICK TEST FOR WRITE 
  lat_mpas=-999.0

  ! Find a SINGLE nearest point from a set of matched pairs.
  do iC= 1, nC
    dist_min=999. ! init
    idx_min=999   ! init
    do iS = 1, cnt_match(iC)
      !measure a distance between Satellite point and MPAS point,
      ! then, find closest pair
      dist = ageometry%distance(lon_mpas_dp(iC),lat_mpas_dp(iC), &
             field_dist(iS,nF_mx+1,iC),field_dist(iS,nF_mx+2,iC))
      if (dist .lt. dist_min) then !update
         dist_min=dist
         idx_min=iS
      end if
    end do
    !Assign !BJJ for test
    if (idx_min.ne.999) then
      lat_mpas(iC) = field_dist(idx_min,1,iC)
    end if
  end do !-- nC

  ! write to the existing MPAS file.
  write(6,*) "BJJ Write nC=",nC
  write(6,*) "BJJ min/max @ MPAS=",minval(lat_mpas),maxval(lat_mpas)
  write(6,*) "BJJ min/max @ SATT=", &
             minval(field_s_valid(:,1)),maxval(field_s_valid(:,1))
  call write_to_mpas (nC, lat_mpas) !lat_mpas is temporary working array for quick test.

!--- match the minimum distance pairs
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
