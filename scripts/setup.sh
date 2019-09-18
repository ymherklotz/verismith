#!/bin/sh

sudo yum -y update

sudo yum -y install git gcc gcc-c++ clang zlib-devel tcl-devel python3 \
     graphviz xdot gperf gmp-devel make flex autoconf \
     readline-devel tmux boost libffi-devel bison ncurses-compat-libs

sudo mkdir -p /mnt/tools
sudo mkdir -p /mnt/work

sudo mount /dev/nvme2n1p1 /mnt/tools
sudo mount /dev/nvme1n1p1 /mnt/work

sudo chown -R ec2-user:ec2-user /mnt/tools/home/ec2-user
sudo chown -R ec2-user:ec2-user /mnt/work

curl https://nixos.org/nix/install | sh

{ cat <<EOF
. $HOME/.nix-profile/etc/profile.d/nix.sh

export PATH="/mnt/tools/opt/yosys/master/bin:\${PATH}"
export PATH="\${PATH}:/mnt/tools/bin"
export PATH="\${PATH}:/mnt/tools/opt/Xilinx/14.7/ISE_DS/ISE/bin/lin64"
export PATH="\${PATH}:/mnt/tools/opt/Xilinx/Vivado/2018.3/bin"
export AFL_PATH=/mnt/tools/lib/afl
EOF
} >> $HOME/.bashrc
