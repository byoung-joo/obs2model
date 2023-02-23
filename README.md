obs2mpas
========

This project is part of the cloud direct insertion project.
It leverages the work of Jamie Bresch https://github.com/jamiebresch/obs2ioda to retrieve obs data,
then interpolates to MPAS unstructured mesh.
This repository was initially designed and written by Yonggang G. Yu.

Aim
---
- Interpolate fields from obs to model


To build and run
----------------
source mpas-jedi environment
git clone https://github.com/byoung-joo/obs2model.git
cd obs2model
mkdir build; cd build
cmake ../ ; make -j4
cd ../test_abi_read
./goes2mpas.x


goes2mpas
---------
main.F90
   !----- 0. read namelist ------------------------------------------------------
   !----- 1. read ABI lat/lon & data --------------------------------------------
   !----- 3. read MPAS lat/lon --------------------------------------------------
   !----- 3. build and search kd-tree -------------------------------------------
   !----- 4. re-organize the matching pairs -------------------------------------
   !----- 5. find the neasest pair ----------------------------------------------
   !----- 6. Write the interpolated fields to MPAS file--------------------------
