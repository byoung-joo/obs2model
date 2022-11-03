#!/bin/bash
# usage: 
# sh run.j   0/1/2/3    gen/extract/dH/carrier

if [ $# -ne 1 ]; then
echo "\$# -ne 1, stop"    
exit
fi
icompute=$1
if [ $icompute -le -1 ]; then
 echo "gen def s-d"
elif [ $icompute -le 0 ]; then
 echo "gen def s-f"
elif [ $icompute -eq 1 ]; then
 echo "extract def"
elif [ $icompute -eq 2 ]; then
 echo "comp dH"
elif [ $icompute -eq 3 ]; then
 echo "comp carrier"
elif [ $icompute -eq 11 ]; then
 echo "extract def, ex *.cif"
else
 echo "wrong icompute"
 exit
fi
sh chart_o2m $icompute


exec="./2wps.x"
$exec > a.out
exit
