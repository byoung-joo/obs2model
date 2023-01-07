module  mod_read_mpas

   use netcdf_mod, only: open_netcdf_for_write, close_netcdf, &
      def_netcdf_dims, def_netcdf_var, def_netcdf_end, &
      put_netcdf_var, missing_r

   implicit none
   include 'netcdf.inc'

   integer, parameter  :: r_single = selected_real_kind(6)  ! single precision
   integer, parameter  :: r_double = selected_real_kind(15) ! double precision
   integer, parameter  :: i_byte   = selected_int_kind(1)   ! byte integer
   integer, parameter  :: i_short  = selected_int_kind(4)   ! short integer
   integer, parameter  :: i_long   = selected_int_kind(8)   ! long integer
   integer, parameter  :: i_kind   = i_long                 ! default integer
   integer, parameter  :: r_kind   = r_single               ! default real
!BJJ   integer, parameter  :: r_kind   = r_double               ! default real

   real(r_kind) :: pi, deg2rad, rad2deg

   contains


   subroutine read_mpas_latlon (nC, lon, lat)
     implicit none
     integer(r_kind),           intent(out) :: nC
     real(r_kind), allocatable, intent(out) :: lon(:), lat(:) 
     ! loc
     character(len=256) :: fname
     integer(i_kind) :: ncid, nf_status, dimid, varid
     integer(i_kind) :: istart, icount
     logical :: isfile
     
      fname = '/glade/p/mmm/parc/guerrett/pandac/fixed_input/30km/GFSAna/x1.655362.init.2018-04-15_00.00.00.nc'
      inquire(file=trim(fname), exist=isfile)
      if ( .not. isfile ) then
         write(0,*) 'File not found: '//trim(fname)
         stop
      end if

         nf_status = nf_OPEN(trim(fname), nf_NOWRITE, ncid)
         if ( nf_status == 0 ) then
            write(0,*) 'Reading '//trim(fname)
         end if

   nf_status = nf_INQ_DIMID(ncid, 'nCells', dimid)
   nf_status = nf_INQ_DIMLEN(ncid, dimid, nC)
   if ( nf_status /= 0 ) then
      write(0,*) 'Error reading dimensions'
      stop
   end if

   allocate( lon(nC), lat(nC) )
   istart = 1
   icount = nC
!   allocate(itmp(nC))
   nf_status = nf_INQ_VARID(ncid, 'lonCell', varid)
   nf_status = nf_GET_VARA_REAL(ncid, varid, istart, icount, lon(:))
   nf_status = nf_INQ_VARID(ncid, 'latCell', varid)
   nf_status = nf_GET_VARA_REAL(ncid, varid, istart, icount, lat(:))

   pi = acos(-1.0)
   deg2rad = pi/180.0
   rad2deg = 1.0/deg2rad

   !lat=lat*rad2deg
   !lon=lon*rad2deg

   nf_status = nf_CLOSE(ncid)

 end subroutine read_mpas_latlon

 end module mod_read_mpas
