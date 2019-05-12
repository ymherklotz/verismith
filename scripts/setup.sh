#!/bin/sh

sudo yum -y update

sudo yum -y install git gcc gcc-c++ tcl-devel python3 \
     graphviz xdot gperf gmp-devel make flex autoconf \
     readline-devel tmux boost libffi-devel bison ncurses-compat-libs

sudo mkdir -p /mnt/tools
sudo mkdir -p /mnt/work

sudo mount /dev/nvme2n1p1 /mnt/tools
sudo mount /dev/nvme1n1p1 /mnt/work

sudo chown -R ec2-user:ec2-user /mnt/tools/home/ec2-user
sudo chown -R ec2-user:ec2-user /mnt/work

curl -sSL https://get.haskellstack.org/ | sh

{ cat <<EOF
export PATH="/mnt/tools/opt/yosys/master/bin:\${PATH}"
export PATH="\${PATH}:/mnt/tools/bin"
export PATH="\${PATH}:/mnt/tools/opt/Xilinx/14.7/ISE_DS/ISE/bin/lin64"
export PATH="\${PATH}:/mnt/tools/opt/Xilinx/Vivado/2018.3/bin"
EOF
} >> $HOME/.bashrc

source $HOME/.bashrc
