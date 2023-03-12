#!/bin/bash
set -euo pipefail
shopt -s nullglob

scripts_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
source "${scripts_dir}/common.sh"

selector="${1}"
shift

cachet_file="${cachet_dir}/${selector}.cachet"
bpl_file="${bpl_dir}/${selector}.bpl"
echo "==> ${cachet_file} -> ${bpl_file}"
mk_parent_dir "${bpl_file}"

compile_log_file_rel="${selector}.log"
compile_log_file="${all_compile_logs_dir}/${compile_log_file_rel}"
successful_compile_log_link="${successful_compile_logs_dir}/${compile_log_file_rel}"
failed_compile_log_link="${failed_compile_logs_dir}/${compile_log_file_rel}"

mk_parent_dir "${compile_log_file}"

rm -f -- "${compile_log_file}"
rm -f -- "${successful_compile_log_link}"
rm -f -- "${failed_compile_log_link}"

if "${bin_dir}/cachet-compiler" "${cachet_file}" --bpl "${bpl_file}" 2>&1 \
    | tee "${compile_log_file}"; then
  mk_parent_dir "${successful_compile_log_link}"
  ln -s "${compile_log_file}" "${successful_compile_log_link}"

  cat "${hacks_bpl_file}" "${bpl_file}" | sponge "${bpl_file}"
  "${bin_dir}/bpl-tree-shaker" -i "${bpl_file}" -t '#JSOp' -p '#MASM^Op'
else
  mk_parent_dir "${failed_compile_log_link}"
  ln -s "${compile_log_file}" "${failed_compile_log_link}"

  echo
fi
