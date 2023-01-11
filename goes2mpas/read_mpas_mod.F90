module  mod_read_mpas

   use netcdf
   implicit none

   integer, parameter  :: r_single = selected_real_kind(6)  ! single precision
   integer, parameter  :: r_double = selected_real_kind(15) ! double precision
   integer, parameter  :: i_byte   = selected_int_kind(1)   ! byte integer
   integer, parameter  :: i_short  = selected_int_kind(4)   ! short integer
   integer, parameter  :: i_long   = selected_int_kind(8)   ! long integer
   integer, parameter  :: i_kind   = i_long                 ! default integer
   integer, parameter  :: r_kind   = r_single               ! default real
!BJJ   integer, parameter  :: r_kind   = r_double               ! default real

   contains


   subroutine read_mpas_latlon (nC, lon, lat)
   implicit none
   integer(i_kind),           intent(out) :: nC
   real(r_kind), allocatable, intent(out) :: lon(:), lat(:) 
   ! loc
   character(len=256) :: fname
   integer(i_kind) :: ncid, nf_status, dimid, varid
   logical :: isfile
     
   fname = '/glade/p/mmm/parc/guerrett/pandac/fixed_input/30km/GFSAna/x1.655362.init.2018-04-15_00.00.00.nc'
   inquire(file=trim(fname), exist=isfile)
   if ( .not. isfile ) then
      write(0,*) 'File not found: '//trim(fname)
      stop
   end if

   nf_status = nf90_OPEN(trim(fname), nf90_NOWRITE, ncid)
   if ( nf_status == 0 ) then
      write(0,*) 'Reading '//trim(fname)
   end if

   nf_status = nf90_inq_dimid(ncid, "nCells", dimid)
   nf_status = nf90_inquire_dimension(ncid, dimid, len=nC)
   if ( nf_status /= 0 ) then
      write(0,*) 'Error reading dimensions'
      stop
   end if

   allocate( lon(nC), lat(nC) )
   nf_status = nf90_INQ_VARID(ncid, 'lonCell', varid)
   nf_status = nf90_GET_VAR(ncid, varid, lon(:))
   nf_status = nf90_INQ_VARID(ncid, 'latCell', varid)
   nf_status = nf90_GET_VAR(ncid, varid, lat(:))

   nf_status = nf90_CLOSE(ncid)

   end subroutine read_mpas_latlon

   subroutine write_to_mpas (nC, var)
   implicit none
   integer(i_kind), intent(in) :: nC
   real(r_kind),    intent(in) :: var(nC)
   ! loc
   character(len=256) :: fname
   integer(i_kind) :: ncid, nf_status, dimid(2), varid
   logical :: isfile
     
!----Create New File----------
!   fname = '/glade/scratch/bjung/interp/obs2model_alt/test_abi_read/testwrite2.nc'
!         nf_status = nf90_CREATE(trim(fname), cmode=NF90_NETCDF4, ncid=ncid)
!         if ( nf_status == 0 ) then
!            write(0,*) 'Writing '//trim(fname)
!         end if
!     nf_status = nf90_def_dim(ncid,"nCells",nC,dimid)
!write(0,*) "nf_status.nf90_def_dim=",nf_status
!     nf_status = nf90_put_att(ncid, NF90_GLOBAL, "nCells",nC)
!write(0,*) "nf_status.nf90_put_att=",nf_status
!     nf_status = nf90_def_var(ncid, "TESTVAR",NF90_FLOAT,dimid,varid)
!write(0,*) "nf_status.nf90_def_var=",nf_status
!     nf_status = nf90_def_var_fill(ncid, varid, 0, -999.0)
!write(0,*) "nf_status.nf90_def_var_fill=",nf_status
!     nf_status = nf90_inq_varid(ncid, "TESTVAR",varid)
!write(0,*) "nf_status.nf90_inq_varid=",nf_status
!     nf_status = nf90_put_var(ncid,varid,var)
!write(0,*) "nf_status.nf90_put_var=",nf_status
!     nf_status = nf90_close(ncid)
!write(0,*) "nf_status.nf90_close=",nf_status
!
!----- add new variable to the existing file
   fname = '/glade/scratch/bjung/interp/obs2model_alt/test_abi_read2/testwrite.nc'
   nf_status = nf90_OPEN(trim(fname), mode=nf90_WRITE, ncid=ncid)
   inquire(file=trim(fname), exist=isfile)
   if ( .not. isfile ) then
      write(0,*) 'File not found: '//trim(fname)
      stop
   end if
write(0,*) "nf90_open:",nf_status
   nf_status = nf90_redef(ncid)
write(0,*) "nf90_redef:",nf_status
   nf_status = nf90_inq_dimid(ncid, "nCells", dimid(1))
write(0,*) "nf90_inq_dimid1:",nf_status
   nf_status = nf90_inq_dimid(ncid, "Time", dimid(2))
write(0,*) "nf90_inq_dimid2:",nf_status
   nf_status = nf90_def_var(ncid, "TESTVAR",NF90_FLOAT,dimid,varid)
write(0,*) "nf90_def_var:",nf_status
   nf_status = nf90_enddef(ncid)
write(0,*) "nf90_enddef:",nf_status
   nf_status = nf90_put_var(ncid,varid,reshape(var, (/nC,1/)) )
write(0,*) "nf90_put_var:",nf_status
   nf_status = nf90_close(ncid)
write(0,*) "nf90_close:",nf_status

 end subroutine write_to_mpas

 end module mod_read_mpas
