!------------------------------------------------------------
! main code was initially designed and written
! by Yonggang G. Yu (yuxxx135@umn.edu) 3-Nov-2022, 2-Dec-2022
!------------------------------------------------------------
!
!  driver of the main code
!  
program  main
   use kinds, only : sp, dp
   use control_para, only : tval, pi, rad2deg, deg2rad
   use atlas_module, only: atlas_geometry, atlas_indexkdtree
   use mod_goes_abi, only: Goes_ReBroadcast_converter
   use mod_read_write_mpas, only: read_mpas_latlon, write_to_mpas
   use mod_read_write_indx, only: read_indx, write_indx

   implicit none

   ! local
   integer :: i, j, icnt, istat
   integer :: nml_unit = 81

   integer :: nx, ny, nfield  ! x y dimension of raw satellite data, number of field read from satellite
   integer :: nS, iS  ! nS: total number of raw satellite grid
   real(sp), allocatable :: lon_s(:,:), lat_s(:,:)  ! (nx,ny) direct read from file
   real(sp), allocatable :: field_s(:,:,:)          ! (nx,ny,nfield)
   character(len=64), allocatable :: varname_s(:)   ! (nfield)
   logical,  allocatable :: l_latlon(:,:)           ! (nx,ny)
   integer :: nS_valid ! only contains # of valid points, removing undefined and missing points.
   real(dp), allocatable :: lon_s_valid(:), lat_s_valid(:)  !double precision to work with kd-tree
   real(sp), allocatable :: field_s_valid(:,:)

   !mpas lat/lon and interpolated field
   integer :: nC, iC  ! nC: total number of MPAS grid
   real(sp), allocatable :: lon_mpas(:), lat_mpas(:)       ! to read. depend on the NetCDF file.
   real(dp), allocatable :: lon_mpas_dp(:), lat_mpas_dp(:) ! double precision for kd-tree
   real(sp), allocatable :: field_mpas(:,:)                ! interpolated field_s (nC,nfield)

   !kd-tree
   type(atlas_indexkdtree) :: kd
   type(atlas_geometry) :: ageometry
   integer :: nn, ix   ! nn= number of nearest point, ix= temporary indice for interp_indx
   integer, allocatable :: interp_indx(:,:)
   character(len=256)  :: indx_fname ! file name for indx I/O
   !BJJ distance
   real(dp) :: dist, dist_min
   integer :: idx_min
   !BJJ Count # of matching for a given MPAS cell
   integer, allocatable :: cnt_match(:)
   real(dp), allocatable :: lat_s_dist(:,:), lon_s_dist(:,:) !for re-organize
   real(sp), allocatable :: field_s_dist(:,:,:)
   integer :: max_pair

   !BJJ namelist for nml_main
   character(len=256)  :: f_mpas_latlon
   character(len=256)  :: f_mpas_out
   logical             :: l_read_indx  ! .true.= read pre-calculated interp_indx from NetCDF file, .false.= calculate it
   logical             :: l_write_indx ! .true.= write pre-calculated interp_indx as NetCDF file
   namelist /main_nml/ f_mpas_latlon, f_mpas_out, l_read_indx ,l_write_indx

 
   777 format(2x,(a,2x,i4.4,a,i2.2,a,i2.2,2x,i2.2,a,i2.2,a,i2.2,/))
   call date_and_time(VALUES=tval)
   write (6, 777) 'Date-time: ',tval(1),'-',tval(2),'-',tval(3),tval(5),':',tval(6),':',tval(7)


   !----- 0. read namelist ------------------------------------------------------
   ! initialize namelist variables
   f_mpas_latlon = './x1.655362.init.nc' ! MPAS file path/name to read lat & lon information
   f_mpas_out    = './x1.655362.init.nc' ! MPAS file for writing the interpolated ABI fields
   l_read_indx   = .false.  ! read index and counnt for matching ABI-MPAS pairs
   l_write_indx  = .false.  ! write index and counnt for matching ABI-MPAS pairs

   ! read namelist
   open(unit=nml_unit, file='namelist.obs2model', status='old', form='formatted')
   read(unit=nml_unit, nml=main_nml, iostat=istat)
   write(0,nml=main_nml)
   if ( istat /= 0 ) then
      write(0,*) 'Error reading namelist main_nml'
      stop
   end if
   close(unit=nml_unit)


   !----- 1. read ABI latlon & data ---------------------------------------------
   ! read lon / lat / field for satellite     
   call Goes_ReBroadcast_converter ( lon_s, lat_s, field_s, varname_s, l_latlon )
   nx     = size(field_s, dim=1)
   ny     = size(field_s, dim=2)
   nfield = size(field_s, dim=3)
   write(6,*) 'BJJ, nx, ny, nfield =', nx, ny, nfield
   nS= nx*ny   ! pts from satellite
   call date_and_time(VALUES=tval)
   write (6, 777) 'GOES READ DONE: ',tval(1),'-',tval(2),'-',tval(3),tval(5),':',tval(6),':',tval(7)

   ! count the valid point
   icnt=0
   do i=1,nx
      do j=1,ny
         if(l_latlon(i,j)) icnt=icnt+1  ! use l_latlon for masking
      end do
   end do
   nS_valid=icnt !BJJ used for kd-tree
   write(6,*) 'BJJ, nS, nS_valid =', nS, nS_valid, real(nS_valid)/real(nS)*100.,"[%]"

   ! keep only the valid point
   allocate (lon_s_valid(nS_valid))
   allocate (lat_s_valid(nS_valid))
   allocate (field_s_valid(nS_valid, nfield))
   icnt=0 ! double check
   do i=1,nx
      do j=1,ny
         if( l_latlon(i,j) ) then
            icnt=icnt+1
            lon_s_valid(icnt)=lon_s(i,j)
            lat_s_valid(icnt)=lat_s(i,j)
            field_s_valid(icnt,:)=field_s(i,j,:)
         else
            cycle
         end if
      end do
   end do
   if(icnt .ne. nS_valid) STOP 777 ! sanity check


   !----- 3. read MPAS lat/lon --------------------------------------------------
   ! read lon / lat from MPAS file
   call read_mpas_latlon (f_mpas_latlon, nC, lon_mpas, lat_mpas)  ! unit [radian], single precision
   lon_mpas_dp=lon_mpas ! pass double precision for kd-tree
   lat_mpas_dp=lat_mpas
   do iC=1,nC
      ! convert lon range [0,360] --> [-180, 180] to match with lon_s
      if(lon_mpas_dp(iC).gt.pi) lon_mpas_dp(iC)=lon_mpas_dp(iC)-2.d0*pi
   end do


   !----- 3. build and search kd-tree -------------------------------------------
   ! buid kd-tree
   nn=1  ! number of nearest points !BJJ set "1". No need to be "3"
   allocate (interp_indx(nn, nS_valid))
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


   ! Search the nearest indice. indice can be written and read from NetCDF file.
   if ( .not. l_read_indx ) then
      call date_and_time(VALUES=tval)
      write (6, 777) 'kd%closestPoints start',tval(1),'-',tval(2),'-',tval(3),tval(5),':',tval(6),':',tval(7)
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
            end do
         end if

         !count # of matching. NOTE: this also works even when nn is not "1".
         ix=interp_indx(1,iS)
         cnt_match(ix)=cnt_match(ix)+1
      end do
      call date_and_time(VALUES=tval)
      write (6, 777) 'kd%closestPoints done',tval(1),'-',tval(2),'-',tval(3),tval(5),':',tval(6),':',tval(7)

      if ( l_write_indx ) then
         write(indx_fname,"(A,I1.1,A,I10.10,A,I10.10,A)") "./indx_",nn,"_",nS_valid,"_",nC,".nc"
         call write_indx(indx_fname, nn, nS_valid, nC, interp_indx, cnt_match)
      end if

   else ! read interp_indx & cnt_match

      write(indx_fname,"(A,I1.1,A,I10.10,A,I10.10,A)") "./indx_",nn,"_",nS_valid,"_",nC,".nc"
      call read_indx(indx_fname, nn, nS_valid, nC, interp_indx, cnt_match)

   end if !-- of l_read_indx

   ! check the min/max of pairs
   write(6,*) "BJJ: MIN/MAX of matching pairs = ",minval(cnt_match),maxval(cnt_match)
   max_pair=maxval(cnt_match)


   !----- 4. re-organize the matching pairs -------------------------------------
   ! distribute the matched pairs (re-organize)
   allocate(lon_s_dist(max_pair,nC))
   allocate(lat_s_dist(max_pair,nC))
   allocate(field_s_dist(max_pair,nfield,nC))
   lon_s_dist(:,:)    =-999.0 !init
   lat_s_dist(:,:)    =-999.0 !init
   field_s_dist(:,:,:)=-999.0 !init
   cnt_match(:)=0 !init

   do iS = 1, nS_valid
      ix=interp_indx(1,iS)
      cnt_match(ix)=cnt_match(ix)+1
      lon_s_dist(cnt_match(ix),ix)=lon_s_valid(iS)*deg2rad  !pass as [radian] to make the next step [dist] easier.
      lat_s_dist(cnt_match(ix),ix)=lat_s_valid(iS)*deg2rad
      field_s_dist(cnt_match(ix),1:nfield,ix)=field_s_valid(iS,1:nfield)
   end do
   call date_and_time(VALUES=tval)
   write (6, 777) 're-organize data done',tval(1),'-',tval(2),'-',tval(3),tval(5),':',tval(6),':',tval(7)

   !deallocate
   deallocate(lon_s_valid)
   deallocate(lat_s_valid)
   deallocate(field_s_valid)
   deallocate(interp_indx)


   !----- 5. find the neasest pair ----------------------------------------------
   ! allocate and initialize the field_mpas
   allocate( field_mpas(nC,nfield) )
   field_mpas=-999.0

   ! find a SINGLE nearest point from a set of matching pairs.
   do iC= 1, nC
      dist_min=999. ! init
      idx_min=999   ! init
      do iS = 1, cnt_match(iC)
         ! measure a distance between Satellite point and MPAS point,
         ! then, find closest pair
         dist = ageometry%distance(lon_mpas_dp(iC),lat_mpas_dp(iC),lon_s_dist(iS,iC),lat_s_dist(iS,iC))
         if (dist .lt. dist_min) then ! update
            dist_min=dist
            idx_min=iS
         end if
      end do
      ! assign
      if (idx_min.ne.999) then
         field_mpas(iC,1:nfield) = field_s_dist(idx_min,1:nfield,iC)
      end if
   end do !-- nC
   call date_and_time(VALUES=tval)
   write (6, 777) 'Find the min. distance pairs done:',tval(1),'-',tval(2),'-',tval(3),tval(5),':',tval(6),':',tval(7)

   ! deallocate
   deallocate(cnt_match)
   deallocate(lon_s_dist)
   deallocate(lat_s_dist)
   deallocate(field_s_dist)


   !----- 6. Write the interpolated fields to MPAS file--------------------------
   ! write to the existing MPAS file.
   call write_to_mpas (f_mpas_out, nC, nfield, field_mpas, varname_s)
   call date_and_time(VALUES=tval)
   write (6, 777) 'write_to_mpas done:',tval(1),'-',tval(2),'-',tval(3),tval(5),':',tval(6),':',tval(7)

   ! deallocate
   deallocate(field_mpas)

   write(6,*) "END OF PROGRAM"

end program main
