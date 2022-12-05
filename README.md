[to build and run]

- source mpas-jedi environment
- git clone  -b  feature/simplify  https://github.com/metdyn/obs2model.git
- mkdir build
- cd build; cmake ../obs2model; make -j4
- cd -
- cd obs2model/test_abi_read
- sh run.j



[goes2wps]

main_o2m.F90
   - read namelist input for goes_abi_converter
   - read namelist input for WPS
   - call Goes_ReBroadcast_converter  [error in lon/lat due to SP or DP ? ]
   - interpolation [not tested]

s1. module  mod_goes_abi [goes_abi_mod.F90]
    GOES Rebroadcast (GRB)
    contains
 - subroutine Goes_ReBroadcast_converter (nml_input, ndim_mx, NF_mx, N, NF, lon, lat, F)
 
 - intended input:
   - nf=2
   - fname(1:nf)
 - intended output:
   - N= nx*ny
   - lon / lat(nx, ny)    lambda_theta
   - Field: F(N, nfield) ::  rad (bt) / cm (cloud mask) 

[basiclib]
   - stringut.F90

[unused]
  - sample.f90 [WRF sample file for WPS]
