project(geos2pws)

set( src_read_abi
     netcdf_mod.f90
     goes_abi_converter.f90 )

set( src_wps
     netcdf_mod.f90
     goes_abi_converter.f90
     setvar.F90
     ll2pws.F90)


# Create the executable
add_executable(read_abi.x  ${src_read_abi})
add_executable(2wps.x  ${src_wps} )
target_link_libraries(read_abi.x 
  PUBLIC 
  NetCDF::NetCDF_Fortran)
target_link_libraries(2wps.x 
  PUBLIC
  NetCDF::NetCDF_Fortran
  basic)

