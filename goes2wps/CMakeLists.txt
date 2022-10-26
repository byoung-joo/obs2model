project(geos2pws)

# Create the executable
add_executable(${PROJECT_NAME} main.F90)

# Link the static library from subproject1 using its alias sub::lib1
# Link the header only library from subproject2 using its alias sub::lib2
# This will cause the include directories for that target to be added to this project
target_link_libraries(${PROJECT_NAME}
    abi
)

# Add a library with the above sources
add_library(abi  netcdf_mod.f90 goes_abi_converter.f90 )
