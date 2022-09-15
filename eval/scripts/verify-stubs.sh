#!/bin/bash
set -euo pipefail
shopt -s nullglob

eval_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && cd .. && pwd)"

repo_dir="$(cd -- "${eval_dir}" && cd .. && pwd)"
corral_exe="${repo_dir}/vendor/corral/source/Corral/bin/Release/net5.0/corral"

data_dir="${eval_dir}/data"
bpl_dir="${data_dir}/bpl"

verify_logs_dir="${data_dir}/verify-logs"
all_verify_logs_dir="${verify_logs_dir}/all"
successful_verify_logs_dir="${verify_logs_dir}/successes"
failed_verify_logs_dir="${verify_logs_dir}/failures"

engine_allow_list=(IonIC)

rm -rf -- "${verify_logs_dir}"

# Change to a temporary directory to collect the detritus that Corral leaves behind.
tmp_dir="$(mktemp -d)"
trap 'rm -rf -- "${tmp_dir}"' EXIT 
cd "${tmp_dir}"

for engine_bpl_dir in "${bpl_dir}"/*; do
  engine="$(basename "${engine_bpl_dir}")"
  if [[ " ${engine_allow_list} " =~ " ${engine} " ]]; then
    for cache_kind_bpl_dir in "${engine_bpl_dir}"/*; do
      cache_kind="$(basename "${cache_kind_bpl_dir}")"
      cache_kind_sub_dir="${engine}/${cache_kind}"

      mkdir -p "${cache_kind_bpl_dir}"

      cache_kind_verify_logs_dir="${all_verify_logs_dir}/${cache_kind_sub_dir}"
      cache_kind_successful_verify_logs_dir="${successful_verify_logs_dir}/${cache_kind_sub_dir}"
      cache_kind_failed_verify_logs_dir="${failed_verify_logs_dir}/${cache_kind_sub_dir}"
      mkdir -p "${cache_kind_verify_logs_dir}"

      for bpl_file in "${cache_kind_bpl_dir}"/*; do
        bpl_file_name="$(basename "${bpl_file}")"

        verify_log_file_name="${bpl_file_name%.bpl}.log"
        verify_log_file="${cache_kind_verify_logs_dir}/${verify_log_file_name}"

        echo "${bpl_file}"
        
        if "${corral_exe}" "${bpl_file}" /trackAllVars /recursionBound:4 "${@}" 2>&1 \
            | tee "${verify_log_file}"; then
          if grep "Program has no bugs" "${verify_log_file}" > /dev/null &&
              ! grep "Reached recursion bound" "${verify_log_file}" > /dev/null; then
            verify_result=0
          else
            verify_result=1
          fi
        else
          verify_result=1
        fi

        if [[ "${verify_result}" = 0 ]]; then
          mkdir -p "${cache_kind_successful_verify_logs_dir}"
          ln -s "${verify_log_file}" \
            "${cache_kind_successful_verify_logs_dir}/${verify_log_file_name}"
        else
          mkdir -p "${cache_kind_failed_verify_logs_dir}"
          ln -s "${verify_log_file}" \
            "${cache_kind_failed_verify_logs_dir}/${verify_log_file_name}"
        fi

        echo
      done
    done
  fi
done
