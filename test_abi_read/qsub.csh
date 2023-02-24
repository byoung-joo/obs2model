#!/bin/csh
#PBS -N peakmem
#PBS -A NMMM0015
#PBS -l walltime=03:00:00
#PBS -q regular
#PBS -j oe
#PBS -k eod
#PBS -m abe
#PBS -l select=1:ncpus=1

setenv TMPDIR /glade/scratch/$USER/temp
mkdir -p $TMPDIR

### Load the module environment
source /etc/profile.d/modules.csh
module purge
module unuse /glade/u/apps/ch/modulefiles/default/compilers
setenv MODULEPATH_ROOT /glade/work/jedipara/cheyenne/spack-stack/modulefiles
module use /glade/work/jedipara/cheyenne/spack-stack/modulefiles/compilers
module use /glade/work/jedipara/cheyenne/spack-stack/modulefiles/misc
module load ecflow/5.8.4
module load miniconda/3.9.12

limit stacksize unlimited
module use /glade/work/jedipara/cheyenne/spack-stack/spack-stack-v1/envs/skylab-2.0.0-gnu-10.1.0/install/modulefiles/Core
module load stack-gcc/10.1.0
module load stack-openmpi/4.1.1
module load stack-python/3.9.12
#module available
module load jedi-mpas-env/1.0.0
module list
setenv GFORTRAN_CONVERT_UNIT 'big_endian:101-200'


module load peak_memusage


cd /glade/scratch/bjung/interp/obs2model_alt/test_abi_read

peak_memusage.exe ./goes2mpas.x

