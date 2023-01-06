#!/bin/bash

icompute=1
sh chart_o2m $icompute

exec="../build/goes2wps/2wps.x"
$exec > a.out
cat a.out
exit
