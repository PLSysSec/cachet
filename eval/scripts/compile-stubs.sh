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

cargo build -p cachet-compiler
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
else
  parallel "${scripts_dir}/compile-stub.sh" ::: "${selectors[@]}"
fi

#for engine_cachet_dir in "${cachet_dir}"/*; do
#  engine="$(basename "${engine_cachet_dir}")"
#  if [[ " ${engine_allow_list} " =~ " ${engine} " ]]; then
#    for cache_kind_cachet_dir in "${engine_cachet_dir}"/*; do
#      cache_kind="$(basename "${cache_kind_cachet_dir}")"
#      cache_kind_sub_dir="${engine}/${cache_kind}"
#
#      cache_kind_bpl_dir="${bpl_dir}/${cache_kind_sub_dir}"
#      mkdir -p "${cache_kind_bpl_dir}"
#
#      cache_kind_compile_logs_dir="${all_compile_logs_dir}/${cache_kind_sub_dir}"
#      cache_kind_successful_compile_logs_dir="${successful_compile_logs_dir}/${cache_kind_sub_dir}"
#      cache_kind_failed_compile_logs_dir="${failed_compile_logs_dir}/${cache_kind_sub_dir}"
#      mkdir -p "${cache_kind_compile_logs_dir}"
#
#      for cachet_file in "${cache_kind_cachet_dir}"/*; do
#        cachet_file_name="$(basename "${cachet_file}")"
#
#        bpl_file_name="${cachet_file_name%.cachet}.bpl"
#        bpl_file="${cache_kind_bpl_dir}/${bpl_file_name}"
#
#        compile_log_file_name="${cachet_file_name%.cachet}.log"
#        compile_log_file="${cache_kind_compile_logs_dir}/${compile_log_file_name}"
#
#        echo "${cachet_file} -> ${bpl_file}"
#        
#        if cargo run -p cachet-compiler -- "${cachet_file}" --bpl "${bpl_file}" 2>&1 \
#            | tee "${compile_log_file}"; then
#          mkdir -p "${cache_kind_successful_compile_logs_dir}"
#          ln -s "${compile_log_file}" \
#            "${cache_kind_successful_compile_logs_dir}/${compile_log_file_name}"
#
#          cat "${hacks_bpl_file}" "${bpl_file}" | sponge "${bpl_file}"
#        else
#          mkdir -p "${cache_kind_failed_compile_logs_dir}"
#          ln -s "${compile_log_file}" \
#            "${cache_kind_failed_compile_logs_dir}/${compile_log_file_name}"
#        fi
#      done
#    done
#  fi
#done
