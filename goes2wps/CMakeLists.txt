project(geos2pws)

set( src_read_abi
     netcdf_mod.f90
     goes_abi_converter.f90 )

set( src_wps
     netcdf_mod.f90
     goes_abi_converter.f90
     setvar.F90
     ll2pws.F90             )



# Create the executable
add_executable(read_abi.x  ${src_read_abi})
add_executable(2wps.x  ${src_wps} )
target_link_libraries(read_abi.x PUBLIC NetCDF::NetCDF_Fortran)
target_link_libraries(2pws.x PUBLIC NetCDF::NetCDF_Fortran basiclib::lib_string)



## Add a library with the above sources
#add_library(abi_lib ${_src})
#add_executable(${PROJECT_NAME} main.F90)
#target_link_libraries(abi PUBLIC NetCDF::NetCDF_Fortran) # NetCDF::NetCDF_C)
#target_link_libraries( ${PROJECT_NAME}
#   PUBLIC
#   abi
#   NetCDF::NetCDF_Fortran ) #  NetCDF::NetCDF_C)
