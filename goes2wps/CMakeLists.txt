project(geos2pws)

set( _src
     netcdf_mod.f90
     goes_abi_converter.f90 )

# Add a library with the above sources
add_library(abi ${_src})

# Create the executable
add_executable(${PROJECT_NAME} main.F90)

target_link_libraries(abi PUBLIC NetCDF::NetCDF_Fortran) # NetCDF::NetCDF_C)

target_link_libraries( ${PROJECT_NAME}
   PUBLIC
   abi
   NetCDF::NetCDF_Fortran ) #  NetCDF::NetCDF_C)
