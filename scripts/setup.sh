#!/bin/sh

sudo yum -y update
sudo yum -y install git gcc72 gcc72-c++ tcl-devel python36 \
     graphviz xdot gperf gmp-devel make bison flex autoconf \
     readline-devel clang6.0 tmux boost libffi-devel

sudo mkdir -p /mnt/tools

curl -sSL https://get.haskellstack.org/ | sh

{ cat <<EOF
export PATH="\${PATH}:/mnt/tools/bin"
export PATH="\${PATH}:/mnt/tools/opt/Xilinx/14.7/ISE_DS/ISE/bin/lin64"
export PATH="\${PATH}:/mnt/tools/opt/Xilinx/Vivado/2018.3/bin"
EOF
} >> $HOME/.bashrc

source $HOME/.bashrc
