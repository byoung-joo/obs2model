set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8 -fdefault-double-8  -fconvert=big-endian -ffree-line-length-none -fno-range-check -fbacktrace")

#project(geos2pws)
#
#set( src_read_abi
#     netcdf_mod.f90
#     goes_abi_converter.f90 )
#
#add_executable(read_abi.x  ${src_read_abi})
#target_link_libraries(read_abi.x 
#  PUBLIC 
#  NetCDF::NetCDF_Fortran)
#


set( src_wps
     main_o2m.F90
     netcdf_mod.f90
     setvar.F90 )
#     ll2wps.F90)

#set( src_wps
#     netcdf_mod.f90
#     goes_abi_converter.f90
#     setvar.F90
#     ll2pws.F90)

add_executable(2wps.x  ${src_wps} )
target_link_libraries(2wps.x 
  PUBLIC
  NetCDF::NetCDF_Fortran
  basic
  eckit
  fckit
  atlas_f
)
