#!/bin/bash
set -euo pipefail
shopt -s nullglob

scripts_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
source "${scripts_dir}/common.sh"

if [[ "${#}" = 0 ]]; then
  cachet_files_glob="${default_glob}"
  rm -rf -- "${bpl_dir}"
  rm -rf -- "${compile_logs_dir}"
else
  cachet_files_glob="${1}"
  shift

  rm -f -- "${bpl_dir}/"${cachet_files_glob}.bpl
  rm -f -- "${compile_logs_dir}"/*/${cachet_files_glob}.log
fi

cargo build --bin cachet-compiler --bin bpl-tree-shaker --bin bpl-inliner
echo

selectors=()
for cachet_file in "${cachet_dir}/"${cachet_files_glob}.cachet; do
  cachet_file_rel="${cachet_file#"${cachet_dir}/"}"
  selector="${cachet_file_rel%.cachet}"
  selectors+=("${selector}")
done

if [[ "${#selectors[@]}" = 0 ]]; then
  >&2 echo "Error: Nothing matched \"${cachet_files_glob}\""
  exit 1
fi

time parallel "${scripts_dir}/compile-stub.sh" ::: "${selectors[@]}"
