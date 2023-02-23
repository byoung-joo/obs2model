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
```
source mpas-jedi environment
git clone https://github.com/byoung-joo/obs2model.git
cd obs2model
mkdir build; cd build
cmake ../ ; make -j4
cd ../test_abi_read
./goes2mpas.x
```

goes2mpas
---------
```
main.F90
   - 0. read namelist
   - 1. read ABI lat/lon & data
   - 3. read MPAS lat/lon
   - 3. build and search kd-tree
   - 4. re-organize the matching pairs
   - 5. find the neasest pair
   - 6. Write the interpolated fields to MPAS file
```


namelist.obs2model
------------------
```
&main_nml
  f_mpas_latlon = '' , ! MPAS file path/name to read lat & lon information
  f_mpas_out    = '' , ! MPAS file for writing the interpolated ABI fields
  l_read_indx   = .true. or .false.,   ! read index and counnt for matching ABI-MPAS pairs
  l_write_indx  = .true. or .false.,   ! write index and counnt for matching ABI-MPAS pairs

&data_nml
  This section is the same as https://github.com/jamiebresch/obs2ioda
```
