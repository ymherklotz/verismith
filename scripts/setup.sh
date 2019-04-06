#!/bin/sh

sudo yum -y update
sudo yum -y install git gcc gcc-c++ tcl-devel python3 \
     graphviz xdot gperf gmp-devel make bison flex autoconf \
     tmux

sudo mkdir -p /mnt/tools
sudo mount /dev/sdf1 /mnt/tools

sudo chown -R $USER:$USER /mnt/tools/home/ec2-user

curl -sSL https://get.haskellstack.org/ | sh

{ cat <<EOF
export PATH="\${PATH}:/mnt/tools/bin:/mnt/tools/opt/Xilinx/14.7/ISE_DS/ISE/bin/lin64/"
EOF
} >> $HOME/.bashrc

source $HOME/.bashrc
