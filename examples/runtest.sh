#!/bin/bash

rm -r encryptSys
echo vhdlSim | ghci SimpleDES_HW.hs 

mkdir -p encryptSys/vhdl/work/ghdl
mkdir -p encryptSys/vhdl/forsyde/ghdl
mkdir -p encryptSys/vhdl/encryptSys_lib/ghdl
cp ../lib/forsyde.vhd encryptSys/vhdl/forsyde/
cd encryptSys/vhdl

ghdl -a --work=forsyde --workdir=forsyde/ghdl  forsyde/forsyde.vhd 
ghdl -a -Pforsyde/ghdl --work=encryptSys_lib --workdir=encryptSys_lib/ghdl  encryptSys_lib/encryptSys_lib.vhd

ghdl -c -Pforsyde/ghdl -PencryptSys_lib/ghdl --work=work --workdir=work/ghdl work/*.vhd test/*.vhd -e encryptSys_tb
