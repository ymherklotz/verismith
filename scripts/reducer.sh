#!/usr/bin/env bash

for i in $(seq 0 100); do
  echo "INFO: Run ${i}"
  dir="$(dirname "$(find /mnt/data/projects/verismith/runs/yosys_0.8/run_5 -name 'reduce*' -type f | shuf | head -1)")"
  dest="$(pwd)/run_${i}"
  interesting="${dest}/creduce/interesting.sh"
  vfile="${dest}/creduce/rtl.v"
  orig="$(pwd)"

  echo "INFO: Picked ${dir}"

  cp -r $dir $dest
  mkdir -p "${dest}/creduce"

  if [[ -f "${dest}/reduce_yosys.v" ]]; then
    echo "INFO: Detected crash"
    cat >$interesting <<EOF
/mnt/data/tools/yosys/constant/bin/yosys -q -p "read_verilog rtl.v; synth; write_verilog -noattr syn_yosys.v" >yosys.log 2>&1
grep "vector::_M_range_check:" yosys.log

exit \$?
EOF
  elif [[ -f "${dest}/reduce_identity_yosys.v" ]]; then
    echo "INFO: Detected mis-synthesis"
    cat >$interesting <<EOF
#!/usr/bin/env bash

grep -v -E '^[[:space:]]*reg[[:space:]].*=.*0.*;' rtl.v | grep '^[[:space:]]*reg[[:space:]].*[^,+-]*;'
if [[ \$? -eq 0 ]]; then exit 1; fi

cp "${dest}/equiv_identity_yosys/top.v" .
cp "${dest}/equiv_identity_yosys/proof.sby" .

sed -i 's:/home/vagrant/.cabal/store/ghc-8.6.5/verismith-0.4.0.1-[a-f0-9]*/share:/home/ymherklotz/projects/verismith:' proof.sby

/mnt/data/tools/yosys/constant/bin/yosys -q -p "read_verilog rtl.v; synth; write_verilog -noattr syn_yosys.v" >yosys1.log 2>&1 || exit 1

grep -v "Warning: Resizing cell port" yosys1.log >yosys.log

grep "Warning" yosys.log
greturn=\$?
if [[ \$greturn -eq 0 ]]; then exit 1; fi

sed -i -E 's/((module[0-9]+)|top)/\1_2/' syn_yosys.v
sed -E 's/((module[0-9]+)|top)/\1_1/' rtl.v >syn_identity.v

sby -f proof.sby >symbiyosys.log 2>&1

grep "FAIL" symbiyosys.log >/dev/null 2>&1 || exit 1

exit 0
EOF
  else
    echo "WARNING: No original reduction found"
    continue
  fi

  cp "${dest}/identity/syn_identity.v" "$vfile"
  chmod +x "$interesting"
  cd "${dest}/creduce"
  time creduce --abs-timing --no-c ./interesting.sh rtl.v >creduce.log
  cd "${orig}"
  echo "INFO: Done"
done
