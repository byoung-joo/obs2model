#project(goes2mpas)
#


#message( STATUS "CMAKE_SOURCE_DIR = ${CMAKE_SOURCE_DIR}" )
#MESSAGE(FATAL_ERRORO "Could not find FOO_EXEC.")

#link_directories(${CMAKE_SOURCE_DIR}/basiclib)   # ad-hoc

set( src_goes2mpas
	setvar.F90 
	netcdf_mod.f90
	goes_abi_mod.F90
	read_write_indx_mod.F90
	read_write_mpas_mod.F90
	main.F90
)

add_executable(goes2mpas.x  ${src_goes2mpas} )
target_link_libraries(goes2mpas.x 
  PUBLIC
  NetCDF::NetCDF_Fortran
  eckit
  fckit
  atlas_f
)
