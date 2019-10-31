#!/usr/bin/env bash

for i in $(seq 0 100); do
  echo "INFO: Run ${i}"
  dir="$(dirname "$(find /mnt/data/projects/verismith/runs/yosys_0.8/run_5 -name 'reduce*' -type f | shuf | uniq)")"
  dest="$(pwd)/run_${i}"

  cp -r $dir $dest
  mkdir -p "${dest}/creduce"

  if [[ -f "${dest}/reduce_yosys.v" ]]; then
    echo "INFO: Detected crash"
    echo >"${dest}/creduce/interesting.sh" <<EOF
/mnt/data/tools/yosys/constant/bin/yosys -q -p "read_verilog rtl.v; synth; write_verilog -noattr syn_yosys.v" >yosys.log 2>&1
grep "vector::_M_range_check:" yosys.log

exit $?
EOF
  elif [[ -f "${dest}/reduce_identity_yosys.v" ]]; then
    echo "INFO: Detected mis-synthesis"
    echo >"${dest}/creduce/interesting.sh" <<EOF
#!/usr/bin/env bash

cp "${dest}/equiv_identity_yosys/top.v" .
cp "${dest}/equiv_identity_yosys/proof.sby" .

/mnt/data/tools/yosys/constant/bin/yosys -q -p "read_verilog rtl.v; synth; write_verilog -noattr syn_yosys.v" >yosys1.log 2>&1 || exit 1

grep -v "Warning: Resizing cell port" yosys1.log >yosys.log

grep "Warning" yosys.log
greturn=$?
if [[ $greturn -eq 0 ]]; then exit 1; fi

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

  cp "${dest}/identity/syn_identity.v" "${dest}/creduce/rtl.v"
  creduce --abs-timing --no-c "${dest}/creduce/interesting.sh" "${dest}/creduce/rtl.v"
done
