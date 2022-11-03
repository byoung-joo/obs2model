################################################################################
# obs2model v2 wi ecbuild
################################################################################

cmake_minimum_required( VERSION 3.12 )
project( cdi VERSION 1.0.0 LANGUAGES C CXX Fortran )

option(OPENMP "Build ioda with OpenMP support" ON)

## Ecbuild integration
find_package( ecbuild 3.3.2 REQUIRED )
include( ecbuild_system NO_POLICY_SCOPE )
ecbuild_declare_project()
list( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake )
set( CMAKE_DIRECTORY_LABELS ${PROJECT_NAME} )

## Configuration options
#include( ${PROJECT_NAME}_compiler_flags )
include( GNUInstallDirs )

## Dependencies
#find_package( jedicmake REQUIRED ) # QUIET )
if(OPENMP)
  find_package( OpenMP COMPONENTS C CXX Fortran )
endif()
find_package( MPI REQUIRED COMPONENTS CXX Fortran )
find_package( Boost REQUIRED )
find_package( eckit 1.11.6 REQUIRED COMPONENTS MPI )
find_package( fckit 0.7.0 REQUIRED )
if(OpenMP_FOUND)
    find_package( atlas 0.20.2 REQUIRED COMPONENTS OMP OMP_Fortran )
else()
    find_package( atlas 0.20.2 REQUIRED )
endif()

#find_package( oops 1.0.0 REQUIRED )
#find_package( saber 1.0.0 REQUIRED )
#find_package( ioda 1.0.0 REQUIRED )
#find_package( ufo 1.0.0 REQUIRED )
find_package(NetCDF REQUIRED COMPONENTS Fortran C)


add_subdirectory( basiclib )
add_subdirectory( goes2wps )

#add_subdirectory( test )


## Package Config
ecbuild_install_project( NAME ${PROJECT_NAME} )

ecbuild_print_summary()
