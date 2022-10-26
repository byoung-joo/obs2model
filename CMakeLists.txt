
################################################################################
# obs2model
################################################################################

cmake_minimum_required( VERSION 3.12 )
#project( obs2model VERSION 1.0.0 LANGUAGES C CXX Fortran )
project( obs2model VERSION 1.0.0 LANGUAGES Fortran )
find_package( NetCDF REQUIRED COMPONENTS Fortran )

add_subdirectory( goes2wps )
#add_subdirectory( test )
