#!/bin/bash
set -euo pipefail
shopt -s nullglob

scripts_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
source "${scripts_dir}/common.sh"

selector="${1}"
shift

bpl_file="${bpl_dir}/${selector}.bpl"
echo "==> ${bpl_file}"

verify_log_file_rel="${selector}.log"
verify_log_file="${all_verify_logs_dir}/${verify_log_file_rel}"
successful_verify_log_link="${successful_verify_logs_dir}/${verify_log_file_rel}"
failed_verify_log_link="${failed_verify_logs_dir}/${verify_log_file_rel}"

mk_parent_dir "${verify_log_file}"

rm -f -- "${verify_log_file}"
rm -f -- "${successful_verify_log_link}"
rm -f -- "${failed_verify_log_link}"

# Change to a temporary directory to collect the detritus that Corral leaves behind.
tmp_dir="$(mktemp -d)"
trap 'rm -rf -- "${tmp_dir}"' EXIT 
cd "${tmp_dir}"

if "${corral_exe}" "${bpl_file}" /trackAllVars /recursionBound:4 "${@}" 2>&1 \
    | tee "${verify_log_file}"; then
  if grep "Program has no bugs" "${verify_log_file}" > /dev/null &&
      ! grep "Reached recursion bound" "${verify_log_file}" > /dev/null; then
    verify_result=0
  else
    verify_result=1
  fi
else
  verify_result="${?}"
fi

if [[ "${verify_result}" = 0 ]]; then
  mk_parent_dir "${successful_verify_log_link}"
  ln -s "${verify_log_file}" "${successful_verify_log_link}"
else
  mk_parent_dir "${failed_verify_log_link}"
  ln -s "${verify_log_file}" "${failed_verify_log_link}"
fi

echo
