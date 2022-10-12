#!/bin/bash
set -euo pipefail
shopt -s nullglob

scripts_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)"
source "${scripts_dir}/common.sh"

if [[ "${#}" = 0 ]]; then
  cachet_files_glob=*/*
  rm -rf -- "${bpl_dir}"
  rm -rf -- "${compile_logs_dir}"
else
  cachet_files_glob="${1}"
  shift

  rm -f -- "${bpl_dir}/${allowed_engines_glob}/"${cachet_files_glob}.bpl
  rm -f -- "${compile_logs_dir}"/*/"${allowed_engines_glob}/"${cachet_files_glob}.log
fi

cargo build -p cachet-compiler
echo

for cachet_file in "${cachet_dir}/${allowed_engines_glob}/"${cachet_files_glob}.cachet; do
  cachet_file_rel="${cachet_file#"${cachet_dir}/"}"
  bpl_file_rel="${cachet_file_rel%.cachet}.bpl"
  bpl_file="${bpl_dir}/${bpl_file_rel}"
  mk_parent_dir "${bpl_file}"
  echo "${cachet_file} -> ${bpl_file}"

  compile_log_file_rel="${cachet_file_rel%.cachet}.log"
  compile_log_file="${all_compile_logs_dir}/${compile_log_file_rel}"
  mk_parent_dir "${compile_log_file}"
  if "${bin_dir}/cachet-compiler" "${cachet_file}" --bpl "${bpl_file}" 2>&1 \
      | tee "${compile_log_file}"; then
    successful_compile_log_link="${successful_compile_logs_dir}/${compile_log_file_rel}"
    mk_parent_dir "${successful_compile_log_link}"
    ln -s "${compile_log_file}" "${successful_compile_log_link}"

    cat "${hacks_bpl_file}" "${bpl_file}" | sponge "${bpl_file}"
  else
    failed_compile_log_link="${failed_compile_logs_dir}/${compile_log_file_rel}"
    mk_parent_dir "${failed_compile_log_link}"
    ln -s "${compile_log_file}" "${failed_compile_log_link}"

    echo
  fi
done

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
