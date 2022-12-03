#project(geos2pws)
#


#message( STATUS "CMAKE_SOURCE_DIR = ${CMAKE_SOURCE_DIR}" )
#MESSAGE(FATAL_ERRORO "Could not find FOO_EXEC.")

#link_directories(${CMAKE_SOURCE_DIR}/basiclib)   # ad-hoc

set( src_wps
	setvar.F90 
	netcdf_mod.f90
	goes_abi_mod.F90
	main_o2m.F90
)

add_executable(2wps.x  ${src_wps} )
target_link_libraries(2wps.x 
  PUBLIC
  NetCDF::NetCDF_Fortran
  basic
  eckit
  fckit
  atlas_f
)
