#!/bin/bash

# using tripole SCRIP file created via gen_fixgrid.F90
export srcgrid=Ct_SCRIP_grid.nc
export dstgrid=latlon.0p5_SCRIP.nc 

export method=bilinear
export wgtfile=tripole.mx025.Ct.to.latlon.0p5.$method"_grid.nc"
#echo "mpirun -4 ESMF_RegridWeightGen --ignore_degenerate  --source $srcgrid  --destination $dstgrid --weight $wgtfile --method $method -i --check"
echo "ESMF_RegridWeightGen --ignore_degenerate  --source $srcgrid  --destination $dstgrid --weight $wgtfile --method $method -i --check"

export method=conserve
export wgtfile=tripole.mx025.Ct.to.latlon.0p5.$method"_grid.nc"
#echo "mpirun -4 ESMF_RegridWeightGen --ignore_degenerate  --source $srcgrid  --destination $dstgrid --weight $wgtfile --method $method -i --check"
echo "ESMF_RegridWeightGen --ignore_degenerate  --source $srcgrid  --destination $dstgrid --weight $wgtfile --method $method -i --check"

#
#

# using tripole SCRIP file created via ncl using corners
export srcgrid=Ct_SCRIP.nc
export dstgrid=latlon.0p5_SCRIP.nc

export method=bilinear
export wgtfile=tripole.mx025.Ct.to.latlon.0p5.$method.nc
#echo "mpirun -4 ESMF_RegridWeightGen --ignore_degenerate  --source $srcgrid  --destination $dstgrid --weight $wgtfile --method $method -i --check"
echo "ESMF_RegridWeightGen --ignore_degenerate  --source $srcgrid  --destination $dstgrid --weight $wgtfile --method $method -i --check"

export method=conserve
export wgtfile=tripole.mx025.Ct.to.latlon.0p5.$method"_grid.nc"
#echo "mpirunn -4 ESMF_RegridWeightGen --ignore_degenerate  --source $srcgrid  --destination $dstgrid --weight $wgtfile --method $method -i --check"
echo "ESMF_RegridWeightGen --ignore_degenerate  --source $srcgrid  --destination $dstgrid --weight $wgtfile --method $method -i --check"

