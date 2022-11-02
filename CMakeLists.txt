
################################################################################
# obs2model
################################################################################

cmake_minimum_required( VERSION 3.12 )
#project( obs2model VERSION 1.0.0 LANGUAGES C CXX Fortran )
project( obs2model VERSION 1.0  LANGUAGES C Fortran )

# Global Configuration
# use MPAS cmake dir
list(INSERT CMAKE_MODULE_PATH 0 ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)
set(CMAKE_DIRECTORY_LABELS ${PROJECT_NAME})
include(GNUInstallDirs)

find_package(NetCDF REQUIRED COMPONENTS Fortran C)


add_subdirectory( basiclib )
add_subdirectory( goes2wps )

#add_subdirectory( test )
