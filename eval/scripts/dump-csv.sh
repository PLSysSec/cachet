#!/bin/bash
set -euo pipefail
shopt -s nullglob

scripts_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
source "${scripts_dir}/common.sh"

if [[ "${#}" = 0 ]]; then
  verify_log_files_glob="${default_glob}"
else
  verify_log_files_glob="${1}"
  shift
fi

echo 'Stub ID,Verification Time (seconds),CacheIR Op Count,MASM Op Count,Branch Count'
for verify_log_file in "${successful_verify_logs_dir}/"${verify_log_files_glob}.log; do
  verify_log_file_rel="${verify_log_file#"${successful_verify_logs_dir}/"}"
  selector="${verify_log_file_rel%.log}"
  verification_time="$(
    grep -h -R 'Boogie verification time: ' "${verify_log_file}" | cut -d' ' -f4
  )"

  bpl_file="${bpl_dir}/${selector}.bpl"
  num_cacheir_ops="$(grep -c 'call #CacheIR~[A-Z].\+ConsPcEmitPath' "${bpl_file}")"
  num_masm_ops="$(grep -c "^  emit'.\\+:$" "${bpl_file}")"
  num_branches="$(grep -c '^  goto .\+,.\+;$' "${bpl_file}")"

  stub_id="${selector#*/}"

  echo "${stub_id},${verification_time},${num_cacheir_ops},${num_masm_ops},${num_branches}"
done
